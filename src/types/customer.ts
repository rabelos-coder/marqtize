export type CustomerState = {
  customer: Customer | null
  loading: boolean
  error: string | null
}

export type Customer = {
  id?: string
  userId?: string | null
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
  createdAt?: Date | string
  updatedAt?: Date | string
  deletedAt?: Date | string | null
}

export type CountCustomer = {
  countCustomer: number
}

export type FindManyCustomer = {
  findManyCustomer: Customer[]
}

export type FindByIdCustomer = {
  findByIdCustomer: Customer
}

export type FindBySlugCustomer = {
  findBySlugCustomer: Customer
}

export type FindFirstCustomer = {
  findFirstCustomer: Customer
}

export type CreateCustomer = {
  createCustomer: Customer
}

export type UpdateCustomer = {
  updateCustomer: Customer
}

export type DeleteCustomer = {
  deleteCustomer: boolean
}

export type DeleteManyCustomer = {
  deleteManyCustomer: boolean
}

export type RemoveCustomer = {
  removeCustomer: boolean
}

export type RemoveManyCustomer = {
  removeManyCustomer: boolean
}

export type RestoreCustomer = {
  restoreCustomer: boolean
}

export type RestoreManyCustomer = {
  restoreManyCustomer: boolean
}

type CustomerInput = {
  userId?: string | null
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

export type CreateCustomerInput = {
  data: CustomerInput
}

type CustomerUpdateInput = {
  id: string
} & CustomerInput

export type UpdateCustomerInput = {
  data: CustomerUpdateInput
}
