// This implementation is mostly copied from Binding.scala TodoMVC example:
// https://github.com/ThoughtWorksInc/todo/blob/master/js/src/main/scala/com/thoughtworks/todo/Main.scala
package mhtml.todo
import scala.scalajs.js.JSApp
import scala.xml.Elem
import scala.xml.Node
import scala.collection.breakOut
import cats.implicits._

import mhtml._
import mhtml.implicits.cats._
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.raw.HTMLInputElement
import upickle.default.read
import upickle.default.write

object MhtmlTodo extends JSApp {

  case class Component[D](view: Rx[Node], model: Rx[D])

  class Todo(val title: String, val completed: Boolean)
  object Todo {
    def apply(title: String, completed: Boolean) = new Todo(title, completed)
    def unapply(todo: Todo) = Option((todo.title, todo.completed))
  }

  case class TodoList(text: String, hash: String, items: Rx[Seq[Todo]])

  //
  // Add components for the app
  //

  lazy val header: Component[Option[Todo]] = {
    val newTodo = Var[Option[Todo]](None)
    val onInputKeydown: (KeyboardEvent) => Unit = { event: KeyboardEvent =>
      (event.currentTarget, event.keyCode) match {
        case (input: HTMLInputElement, KeyCode.Enter) =>
          input.value.trim match {
            case "" =>
            case title =>
              newTodo := Some(Todo(title, completed = false))
              input.value = ""
          }
        case _ =>
      }
    }
    val headerNode =
      <header class="header">
        <h1>todos</h1>
        <input class="new-todo"
               autofocus="true"
               placeholder="What needs to be done?"
               onkeydown={onInputKeydown}/>
      </header>
    Component(Rx(headerNode), newTodo)
  }

  def todoListItem(todo: Todo): Component[Option[Todo]] = {
    val removeTodo = Var[Option[Todo]](None)
    val suppressOnBlur = Var(false)
    def submit: Event => Unit = { event: Event =>
      suppressOnBlur := true
      editingTodo := None
      event.currentTarget.asInstanceOf[HTMLInputElement].value.trim match {
        case "" =>
          removeTodo := Some(todo)
        case trimmedTitle =>
          updateTodo(todo, Todo(trimmedTitle, todo.completed))
      }
    }
    def onEditTodoTitle = { event: KeyboardEvent =>
      event.keyCode match {
        case KeyCode.Escape =>
          suppressOnBlur := true
          editingTodo := None
        case KeyCode.Enter =>
          submit(event)
          focusInput()
        case _ =>
      }
    }
    def ignoreEvent: (Event) => Unit = _ => Unit
    def blurHandler: Rx[Event => Unit] =
      suppressOnBlur.map(x => if (x) ignoreEvent else submit)
    def onToggleCompleted: (Event) => Unit = { event: Event =>
      event.currentTarget match {
        case input: HTMLInputElement =>
          updateTodo(todo, Todo(todo.title, input.checked))
        case _ =>
      }
    }
    val onDoubleClick: (Event) => Unit = { _: Event =>
      editingTodo := Some(todo)
      focusInput()
    }
    val onDelete: (Event) => Unit = _ =>
      removeTodo := Some(todo)

    val css = editingTodo.map { x =>
      val editing = if (x.contains(todo)) "editing" else ""
      val completed = if (todo.completed) "completed" else ""
      s"$editing $completed"
    }
    val todoListElem =
      <li class={css}>
        <div class="view">
          <input onclick={onToggleCompleted}
                 class="toggle"
                 type="checkbox"
                 checked={conditionalAttribute(todo.completed)} />
          <label ondblclick={onDoubleClick}>{ todo.title }</label>
          <button onclick={onDelete} class="destroy"></button>
        </div>
        <input onkeydown={onEditTodoTitle}
               id="editInput"
               class="edit"
               value={todo.title}
               onblur={blurHandler}/>
      </li>
    Component(Rx(todoListElem), removeTodo)
  }

//  object Model {
  val LocalStorageName = "todo.mhtml"
  def load(): Seq[Todo] =
    LocalStorage(LocalStorageName).toSeq.flatMap(read[Seq[Todo]])
  def save(todos: Seq[Todo]): Unit =
    LocalStorage(LocalStorageName) = write(todos)
  val allTodos: Var[Seq[Todo]] = ((
    Rx[Seq[Todo]](load()) |@| header.model
  ) map {
    case (stored: Seq[Todo], newTodoMaybe: Option[Todo]) =>
      (stored ++ newTodoMaybe)
  }).asVar //FIXME: asVar

  val autoSave: Unit = allTodos.foreach(save)
  val editingTodo: Var[Option[Todo]] = Var[Option[Todo]](None)
  val all = TodoList("All", "#/", allTodos)
  val active =
    TodoList("Active", "#/active", allTodos.map(_.filter(!_.completed)))
  val completed =
    TodoList("Completed", "#/completed", allTodos.map(_.filter(_.completed)))
  val todoLists = Seq(all, active, completed)

  val windowHash: Rx[String] = Rx(dom.window.location.hash).merge{
    var updatedHash = Var(dom.window.location.hash)
    dom.window.onhashchange({ _ =>
      updatedHash := dom.window.location.hash
    })
    updatedHash
  }
  val currentTodoList: Rx[TodoList] = windowHash.map(hash =>
    todoLists.find(_.hash === hash).getOrElse(all)
  )
  val todoListComponents: Rx[Seq[Component[Option[Todo]]]] = currentTodoList.flatMap { current =>
    current.items.map(_.map(todoListItem))
  }
  //TODO: somehow get this to work with Seq instead of List to avoid conversion:
  val (todoListElems: Rx[List[Node]], todoListRemovals: Rx[List[Todo]]) = todoListComponents.map{tlcSeq =>
//    val elems: List[Rx[Node]] = tlcSeq.map(tlc => tlc.split).map(x => x._1).toList.sequence
//    val data: List[Rx[Node]] = tlcSeq.map(tlc => tlc.split).map(x => x._2).toList.sequence

    //val asf: Seq[Component[Option[Todo]]] = tlcSeq
    val unzippedComponents: (List[Rx[Node]], List[Rx[Option[Todo]]]) =
      tlcSeq.toList.map(tlc => (tlc.view, tlc.model)).unzip
    (unzippedComponents._1.sequence, unzippedComponents._2.sequence)
  }
  todoListRemovals.map{rmList =>
    allTodos.update{curTodos =>
      curTodos.remove(rmList.toSeq)
      curTodos
    }//curTodos.remove(rmList.toSeq)}

  }





//  }

  // We're not strict about having all updates happen here, only the ones that
  // contain some non-trivial logic.
  //object Update {
//  def newTodo(title: String): Unit =
//    allTodos.update(todos => Todo(title, completed = false) +: todos)
//  def removeTodo(todo: Todo): Unit =
//    allTodos.update(_.filterNot(_ eq todo))
  def updateTodo(toUpdate: Todo, newTodo: Todo) =
    allTodos.update(todos => todos.updated(todos.indexOf(toUpdate), newTodo))
  def setAllCompleted(completed: Boolean) =
    allTodos.update(_.map {
      case t if t.completed != completed =>
        Todo(t.title, completed)
      case t => t
    })
  //}

  //object View {

  // helper to render <input checked={null}> as <input>
  def conditionalAttribute(cond: Boolean) = if (cond) "true" else null



  def focusInput() = dom.document.getElementById("editInput") match {
    case t: HTMLInputElement => t.focus()
    case _ =>
  }



  def toggleAllClickHandler = { event: Event =>
    event.currentTarget match {
      case input: HTMLInputElement =>
        setAllCompleted(input.checked)
      case _ =>
    }
  }

  def mainSection: Node = {

    // TODO(olafur) This is broken in 0.1, fix here https://github.com/OlivierBlanvillain/monadic-html/pull/9
    val checked = active.items.map(x => conditionalAttribute(x.isEmpty))
    val display = allTodos.map(todos => if (todos.isEmpty) "none" else "")
    <section class="main" style:display={display}>
      <input onclick={toggleAllClickHandler}
             type="checkbox"
             class="toggle-all"
             checked={checked} />
      <label for="toggle-all" checked={checked}>Mark all as complete</label>
      <ul class="todo-list">{todoListElems}</ul>
    </section>
  }

  val count = active.items.map { items =>
    <span class="todo-count">
      <strong>{ items.length }</strong>
      {if (items.length === 1) "item" else "items"} left
    </span>
  }

  def todoListsFooter(todoList: TodoList) = {
    val css = currentTodoList.map(x => if (x === todoList) "selected" else "")
    <li>
      <a href={ todoList.hash } class={css}>{ todoList.text }</a>
    </li>
  }

  def footer: Node = {
    def onClearCompleted = { _: Event =>
      allTodos.update(_.filterNot(_.completed))
    }
    val display = allTodos.map(x => if (x.isEmpty) "none" else "")
    val visibility =
      completed.items.map(x => if (x.isEmpty) "hidden" else "visible")
    <footer class="footer" style:display={display}>
      <ul class="filters">{todoLists.map(todoListsFooter)}</ul>
      <button onclick={onClearCompleted}
              class="clear-completed"
              style:visibility={visibility}>
        Clear completed
      </button>
    </footer>
  }

  lazy val todoapp: Node = {
    <div>
      <section class="todoapp">{header.view}{mainSection}{footer}</section>
      <footer class="info">
        <p>Double-click to edit a todo</p>
        <p>
          Originally written by <a href="https://github.com/atry">Yang Bo</a>,
          adapted to monadic-html by <a href="https://github.com/olafurpg">Olafur Pall Geirsson</a>.
        </p>
        <p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
      </footer>
    </div>
  }
  //}

  def main(): Unit = {
    val div = dom.document.getElementById("application-container")
    mount(div, todoapp)
  }
}
