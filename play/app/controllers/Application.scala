package controllers

import javax.inject.Inject

import org.webjars.play.RequireJS
import play.api._
import play.api.mvc._

class Application @Inject() (
  implicit rjs: RequireJS
) extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

}
