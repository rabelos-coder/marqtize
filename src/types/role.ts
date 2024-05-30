import { Account } from './account'
import { PaginatedObject } from './common'
import { User } from './user'

export type Role = {
  id: number
  accountId: number
  name: string
  slug: string
  isDeleteable?: boolean
  isDefault?: boolean
  claims: string[]
  createdAt: string
  updatedAt: string
  deletedAt?: string

  users?: User[]
  account?: Account
}

export type CountRole = {
  countRole: number
}

export type FindManyRole = {
  findManyRole: Role[]
}

export type FindByIdRole = {
  findByIdRole: Role
}

export type FindFirstRole = {
  findFirstRole: Role
}

export type CreateRole = {
  createRole: Role
}

export type UpdateRole = {
  updateRole: Role
}

export type DeleteRole = {
  deleteRole: boolean
}

export type DeleteManyRole = {
  deleteManyRole: boolean
}

export type RemoveRole = {
  removeRole: boolean
}

export type RemoveManyRole = {
  removeManyRole: boolean
}

export type RestoreRole = {
  restoreRole: boolean
}

export type RestoreManyRole = {
  restoreManyRole: boolean
}

export type RoleInput = {
  accountId?: number | null
  name: string
  slug: string
  isDefault?: boolean
  claims?: string[]
  users?: number[]
}

export type CreateRoleInput = {
  data: RoleInput
}

type RoleUpdateInput = {
  id: number
} & RoleInput

export type UpdateRoleInput = {
  data: RoleUpdateInput
}

export type PaginatedRole = {
  paginatedRole: PaginatedObject<Role>
}
