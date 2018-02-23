package code.annotation

import code.annotation._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class UserAdminService(api: ApiService) {
  def fetchAllUsers(): Future[Seq[AnnotationUser]] = {
    val cmd = AnnotationUserCommand(
      AnnotationUserCommand.Command.List(
        AllUsers()
      )
    )
    api.userListCommand(cmd)
  }

  def addUser(name: String): Future[Seq[AnnotationUser]] = {
    val cmd = AnnotationUserCommand(
      AnnotationUserCommand.Command.Add(
        AddUser(name)
      )
    )
    api.userListCommand(cmd)
  }

  def changeAdmin(user: AnnotationUser): Future[Seq[AnnotationUser]] = {
    val cmd = AnnotationUserCommand(
      AnnotationUserCommand.Command.Admin(
        ChangeAdminStatus(user.id, !user.admin)
      )
    )
    api.userListCommand(cmd)
  }
}

object UserList {

  class UserListBackend(scope: BackendScope[ApiService, Seq[AnnotationUser]]) {
    private val service = new UserAdminService(scope.props.runNow())

    def init() = Callback.future {
      val users = service.fetchAllUsers()
      users.map { u =>
        scope.modState(_ => u)
      }
    }

    private def replaceUsers(users: Seq[AnnotationUser]) = {
      scope.modState(_ => users)
    }

    private def adminLink(u: AnnotationUser) = {
      <.a(
        ^.cls := "admin-link",
        ^.onClick --> Callback.future { service.changeAdmin(u).map(lst => replaceUsers(lst)) },
        if (u.admin) "Admin" else "Annotator"
      )
    }

    private def renderUserAsRow(u: AnnotationUser) = {
      <.tr(
        ^.key := u.id,
        <.td(u.id),
        <.td(u.name),
        <.td(u.token),
        <.td(adminLink(u))
      )
    }

    def render(users: Seq[AnnotationUser]) = {
      val snap = StateSnapshot(users).setStateVia(scope)

      <.div(
        <.h2("Current User List"),
        AddUserWidget((service, snap)),
        <.table(
          <.thead(
            <.tr(
              <.td("Id"),
              <.td("Name"),
              <.td("Token"),
              <.td("Admin")
            )
          ),
          <.tbody(
            users.map(renderUserAsRow).toVdomArray
          )
        )
      )
    }
  }

  val List = ScalaComponent
    .builder[ApiService]("UserList")
    .initialState[Seq[AnnotationUser]](Nil)
    .backend(p => new UserListBackend(p))
    .renderBackend
    .componentDidMount(_.backend.init())
    .build

  val AddUserWidget = ScalaComponent
    .builder[(UserAdminService, StateSnapshot[Seq[AnnotationUser]])]("AddUserWidget")
    .initialState(AnnotationUser())
    .noBackend
    .render { s =>
      import Lenses._

      val (svc, cb) = s.props

      <.div(
        ^.cls := "add-user-widget",
        Edits.Field(("New User Name", s.zoom(_.name))),
        <.button(
          ^.value := "Add",
          ^.onClick --> Callback.future { svc.addUser(s.state.name).map(x => cb.setState(x)) }
        )
      )
    }
    .build
}
