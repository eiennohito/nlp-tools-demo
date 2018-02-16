package controllers

import javax.inject.Inject

import org.webjars.play.RequireJS
import play.api.mvc.InjectedController


class Application @Inject() (
  implicit rjs: RequireJS
) extends InjectedController {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

}
