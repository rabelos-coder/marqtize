import { Upload } from './common'
import { Customer } from './customer'
import { UserType, UserTypeEnum } from './enums'
import { Language } from './language'
import { Role } from './role'
import { Timezone } from './timezone'

export type User = {
  id: string
  customerId: string
  name: string
  systemName: string
  email: string
  emailVerified: string
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
  customer: Customer
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

type UserInput = {
  customerId?: string
  name: string
  systemName: string
  email: string
  password: string
  image?: string
  imageFile?: Upload
  isActive: boolean
  isSuperAdmin: boolean
  language: string
  type: UserTypeEnum
  timezoneId: string
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
