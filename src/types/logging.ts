import { PaginatedObject } from './common'
import { User } from './user'

export type Logging = {
  id: number
  userId?: number
  tokenId?: number
  method: string
  operation?: string
  endpoint: string
  ipAddress?: string
  origin?: string
  userAgent?: string
  requestData?: string
  responseData?: string
  createdAt: string
  updatedAt: string

  user?: User
}

export type CountLogging = {
  countLogging: number
}

export type FindManyLogging = {
  findManyLogging: Logging[]
}

export type FindByIdLogging = {
  findByIdLogging: Logging
}

export type FindFirstLogging = {
  findFirstLogging: Logging
}

export type DeleteLogging = {
  deleteLogging: boolean
}

export type DeleteManyLogging = {
  deleteManyLogging: boolean
}

export type PaginatedLogging = {
  paginatedLogging: PaginatedObject<Logging>
}
