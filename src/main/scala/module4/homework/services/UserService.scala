package module4.homework.services

import io.getquill.{EntityQuery, Quoted}
import zio.Has
import module4.homework.dao.entity.{Role, RoleCode, User, UserToRole}
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db

@accessible
object UserService {
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service {
        val dc = db.Ctx
        import dc._

        lazy val roleSchema: Quoted[EntityQuery[Role]] = quote {
            querySchema[Role](""""Role"""")
        }

        lazy val userToRoleSchema: Quoted[EntityQuery[UserToRole]] = quote {
            querySchema[UserToRole](""""UserToRole"""")
        }

        def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()

        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = {
            for {
                users <- listUsers()
                dtos  <- listUserDtos(users)
            } yield dtos
        }
        
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = {
            val userId = user.typedId
            val tr = transaction {
                for {
                    _ <- userRepo.createUser(user)
                    _ <- userRepo.insertRoleToUser(roleCode, userId)
                } yield ()
            }
            for {
                _         <- tr
                userRoles <- userRepo.userRoles(userId).map(_.toSet)
            } yield UserDTO(user, userRoles)

        }
        
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = {
            for {
                users <- userRepo.listUsersWithRole(roleCode)
                dtos <- listUserDtos(users)
            } yield dtos
        }

        private def listUserDtos(users: List[User]): RIO[db.DataSource,List[UserDTO]] = {
            ZIO.foreach(users) { user =>
                userRepo.userRoles(user.typedId).map(roles => UserDTO(user = user, roles = roles.toSet))
            }
        }
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = {
        ZLayer.fromService[UserRepository.Service, UserService.Service](service => new Impl(service))
    }
}

case class UserDTO(user: User, roles: Set[Role])