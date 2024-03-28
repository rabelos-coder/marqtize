import { Account } from './account'
import { UserAccount, UserSession } from './auth'
import { PaginatedObject } from './common'
import { UserType, UserTypeEnum } from './enums'
import { Language } from './language'
import { Role } from './role'
import { Timezone } from './timezone'

export type User = {
  id: string
  accountId: string
  name: string
  systemName: string
  email: string
  password: string
  isActive: boolean
  isSuperAdmin: boolean
  image: string
  type: UserType
  language: Language
  resetToken: string
  resetTokenExpires: string
  twoFactorSecret: string
  twoFactorRecoveryCodes: string
  twoFactorConfirmedAt: string
  timezoneId: string
  createdAt: string
  updatedAt: string
  deletedAt: string

  claims: string[]
  roles: Role[]
  timezone: Timezone
  account: Account
  userAccounts: UserAccount[]
  userSessions: UserSession[]
}

export type CountUser = {
  countUser: number
}

export type FindManyUser = {
  findManyUser: User[]
}

export type FindByIdUser = {
  findByIdUser: User
}

export type FindFirstUser = {
  findFirstUser: User
}

export type CreateUser = {
  createUser: User
}

export type UpdateUser = {
  updateUser: User
}

export type DeleteUser = {
  deleteUser: boolean
}

export type DeleteManyUser = {
  deleteManyUser: boolean
}

export type RemoveUser = {
  removeUser: boolean
}

export type RemoveManyUser = {
  removeManyUser: boolean
}

export type RestoreUser = {
  restoreUser: boolean
}

export type RestoreManyUser = {
  restoreManyUser: boolean
}

export type UserInput = {
  accountId?: string | null
  name: string
  systemName: string
  email: string
  password?: string | null
  image?: string | null
  imageFile?: File | null
  removeImage?: boolean
  isActive: boolean
  isSuperAdmin: boolean
  language: string
  type: UserTypeEnum
  timezoneId?: string | null
  claims?: string[]
  roles?: string[]
}

export type CreateUserInput = {
  data: UserInput
}

type UserUpdateInput = {
  id: string
} & UserInput

export type UpdateUserInput = {
  data: UserUpdateInput
}

export type PaginatedUser = {
  paginatedUser: PaginatedObject<User>
}
