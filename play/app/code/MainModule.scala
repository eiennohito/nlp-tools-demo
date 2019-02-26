package code

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import code.annotation.AnnotationDbModule
import code.grpc.LatticeDumpJppModule
import com.google.inject.{Binder, Module, Provides, Singleton}

/**
  * @author eiennohito
  * @since 2016/09/28
  */
class MainModule extends Module {
  override def configure(binder: Binder): Unit = {
    binder.install(new JumanppModule)
    binder.install(new MongoModule)
    binder.install(new AnnotationDbModule)
    binder.install(new LatticeDumpJppModule)
    binder.install(new AllowedFieldsModule)
  }

  @Provides
  @Singleton
  def amat(asys: ActorSystem): ActorMaterializer = {
    ActorMaterializer.create(asys)
  }
}
