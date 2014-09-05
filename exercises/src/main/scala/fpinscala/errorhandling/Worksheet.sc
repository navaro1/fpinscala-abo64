//package fpinscala.errorhandling

object Worksheet {
  def parseInteger(str: String): Option[Int] = {
    try {
      Option(str).map(_.toInt)
    } catch {
      case e: NumberFormatException => None
    }
// Scala 2.10's Try comes in handy here
//    scala.util.Try { str.toInt } toOption
  }                                               //> parseInteger: (str: String)Option[Int]

  parseInteger("42")                              //> res0: Option[Int] = Some(42)
  parseInteger(null)                              //> res1: Option[Int] = None
  parseInteger("forty two")                       //> res2: Option[Int] = None
  parseInteger("42").getOrElse(43)                //> res3: Int = 42
  parseInteger("forty two").getOrElse(43)         //> res4: Int = 43

  println("-" * 50)                               //> --------------------------------------------------

  case class Request(loginId: String, password: String)

  def nonEmpty(str: String): Option[String] = Option(str) filter(!_.isEmpty)
                                                  //> nonEmpty: (str: String)Option[String]
  def getLoginId(request: Request) = nonEmpty(request.loginId)
                                                  //> getLoginId: (request: Worksheet.Request)Option[String]
  def getPassword(request: Request) = nonEmpty(request.password)
                                                  //> getPassword: (request: Worksheet.Request)Option[String]
  getLoginId(Request("myId", "myPassword"))       //> res5: Option[String] = Some(myId)
  getLoginId(Request(null, "myPassword"))         //> res6: Option[String] = None
  getLoginId(Request("", "myPassword"))           //> res7: Option[String] = None

  // ok, no potential NPE here as Scala implemented the == operator properly also for Strings,
  // but you could also use Java code for the NPE: loginId.equals("myId") && password.equals("myPassword")
  def login(loginId: String, password: String) =
    loginId == "myId" && password == "myPassword" //> login: (loginId: String, password: String)Boolean

  def tryLogin(request: Request): Option[Boolean] = {
//    val loginId = getLoginId(request)
//    val password = getPassword(request)
//    loginId.flatMap(id =>
//      password.map(pw =>
//        login(id, pw)))
    for {
      loginId <- getLoginId(request)
      password <- getPassword(request)
    } yield login(loginId, password)
  }                                               //> tryLogin: (request: Worksheet.Request)Option[Boolean]

  tryLogin(Request("myId", "myPassword"))         //> res8: Option[Boolean] = Some(true)
  tryLogin(Request("wrongId", "myPassword"))      //> res9: Option[Boolean] = Some(false)
  tryLogin(Request("myId", "wrongPassword"))      //> res10: Option[Boolean] = Some(false)
  tryLogin(Request(null, "myPassword"))           //> res11: Option[Boolean] = None
  tryLogin(Request("", "myPassword"))             //> res12: Option[Boolean] = None
  tryLogin(Request("myId", null))                 //> res13: Option[Boolean] = None
  tryLogin(Request(null, null))                   //> res14: Option[Boolean] = None


  // lift of f into Option monad
  def lift[A,B,C](f: (A,B) => C): (Option[A], Option[B]) => Option[C] = {
    (option1: Option[A], option2: Option[B]) =>
      for {
        arg1 <- option1
        arg2 <- option2
      } yield f(arg1, arg2)
  }                                               //> lift: [A, B, C](f: (A, B) => C)(Option[A], Option[B]) => Option[C]
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C) = lift(f)(oa,ob)
                                                  //> map2: [A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C)Option[C]

  def tryLogin1(request: Request) = {
    lift(login)(getLoginId(request), getPassword(request))
//    map2(getLoginId(request), getPassword(request))(login)
  }                                               //> tryLogin1: (request: Worksheet.Request)Option[Boolean]

  tryLogin1(Request("myId", "myPassword"))        //> res15: Option[Boolean] = Some(true)
  tryLogin1(Request("wrongId", "myPassword"))     //> res16: Option[Boolean] = Some(false)
  tryLogin1(Request("myId", "wrongPassword"))     //> res17: Option[Boolean] = Some(false)
  tryLogin1(Request(null, "myPassword"))          //> res18: Option[Boolean] = None
  tryLogin1(Request("", "myPassword"))            //> res19: Option[Boolean] = None
  tryLogin1(Request("myId", null))                //> res20: Option[Boolean] = None
  tryLogin1(Request(null, null))                  //> res21: Option[Boolean] = None

  println("-" * 50)                               //> --------------------------------------------------

  case class UserPK(value: Long)
  case class User(userPK: UserPK, email: String)

  type DbUser = Either[String, User]

  val userDb: UserPK => DbUser = {
    val random = new scala.util.Random
    def badLuck: Boolean = random.nextInt(10) >= 7
    def fate: Either[String, String] = if (badLuck) Left("bad luck!") else Right("good luck!")
    val users: Map[UserPK,User] =
      Seq(User(UserPK(1L),"han@star.wars"), User(UserPK(2L),"darth@star.wars"))
        .map{u => (u.userPK, u)}.toMap
    def getUser(userPK: UserPK): DbUser = {
      val result =
//        if (badLuck) Left("bad luck!") else
        fate.fold(
          Left(_),
          _ => users.get(userPK).map(Right(_)).getOrElse(Left(s"not found: $userPK"))
        )
      println(s"userDB: $userPK -> $result")
      result
    }
    getUser
  }                                               //> userDb  : Worksheet.UserPK => Worksheet.DbUser = <function1>

  // Java style: throw an Exception if it's not Right
  def javaStyle[A](either: Either[_, A]): A =
    either.fold(left => throw new Exception(left.toString), identity[A])
                                                  //> javaStyle: [A](either: Either[_, A])A
//    either.right.getOrElse(throw new Exception(either.left.get.toString))

  // we are not interested in the error message, so just throw it away
  def toOption[A](either: Either[_, A]): Option[A] = either.right.toOption
                                                  //> toOption: [A](either: Either[_, A])Option[A]

  def retrieveUser(userPK: UserPK) =
    userDb(userPK)                                //> retrieveUser: (userPK: Worksheet.UserPK)Worksheet.DbUser
//    javaStyle(userDb(userPK))
//    toOption(userDb(userPK))

  retrieveUser(UserPK(2L))                        //> userDB: UserPK(2) -> Right(User(UserPK(2),darth@star.wars))
                                                  //| res22: Worksheet.DbUser = Right(User(UserPK(2),darth@star.wars))
  retrieveUser(UserPK(3L))                        //> userDB: UserPK(3) -> Left(not found: UserPK(3))
                                                  //| res23: Worksheet.DbUser = Left(not found: UserPK(3))

  def displayUserEmail(userId: Long): String = {
    def retrieveUserEmail: Option[String] = {
      for {
//      validUserId <- Some(userId) filter (_ > 0)
        validUserId <- Some(userId) if userId > 0
        userPK = UserPK(validUserId)
        user <- toOption(retrieveUser(userPK))
      } yield user.email
    }

    retrieveUserEmail getOrElse "sorry!"
  }                                               //> displayUserEmail: (userId: Long)String

  displayUserEmail(-1L)                           //> res24: String = sorry!
  displayUserEmail(1L)                            //> userDB: UserPK(1) -> Right(User(UserPK(1),han@star.wars))
                                                  //| res25: String = han@star.wars
  displayUserEmail(3L)                            //> userDB: UserPK(3) -> Left(bad luck!)
                                                  //| res26: String = sorry!

}