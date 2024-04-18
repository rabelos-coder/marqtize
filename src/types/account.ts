import { PaginatedObject } from './common'

export type AccountState = {
  account: Account | null
  loading: boolean
  error: string | null
}

export type Account = {
  id: string
  erpId: string | null
  corporateNumber: string
  corporateName: string
  tradingName: string
  systemName: string
  email: string
  slug: string
  phone: string | null
  mobile: string | null
  postcode: string | null
  address: string | null
  number: string | null
  complement: string | null
  neighborhood: string | null
  countryId: string | null
  cityId: string | null
  stateId: string | null
  isActive: boolean
  tradingLogo: string | null
  createdAt: string
  updatedAt: string
  deletedAt: string | null
}

export type CountAccount = {
  countAccount: number
}

export type FindManyAccount = {
  findManyAccount: Account[]
}

export type FindByIdAccount = {
  findByIdAccount: Account
}

export type FindBySlugOrHostAccount = {
  findBySlugOrHostAccount: Account | null
}

export type FindBySlugOrHostInput = {
  slug?: string
  host?: string
}

export type FindFirstAccount = {
  findFirstAccount: Account
}

export type CreateAccount = {
  createAccount: Account
}

export type UpdateAccount = {
  updateAccount: Account
}

export type DeleteAccount = {
  deleteAccount: boolean
}

export type DeleteManyAccount = {
  deleteManyAccount: boolean
}

export type RemoveAccount = {
  removeAccount: boolean
}

export type RemoveManyAccount = {
  removeManyAccount: boolean
}

export type RestoreAccount = {
  restoreAccount: boolean
}

export type RestoreManyAccount = {
  restoreManyAccount: boolean
}

type AccountInput = {
  erpId?: string | null
  corporateNumber?: string | null
  corporateName: string
  tradingName?: string | null
  systemName: string
  email: string
  slug: string
  phone?: string | null
  mobile?: string | null
  postcode?: string | null
  address?: string | null
  number?: string | null
  complement?: string | null
  neighborhood?: string | null
  countryId?: string | null
  cityId?: string | null
  stateId?: string | null
  isActive?: boolean
  tradingLogo?: string | null
}

export type CreateAccountInput = {
  data: AccountInput
}

type AccountUpdateInput = {
  id: string
} & AccountInput

export type UpdateAccountInput = {
  data: AccountUpdateInput
}

export type PaginatedAccount = {
  paginatedAccount: PaginatedObject<Account>
}
