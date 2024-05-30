import { ReactNode } from 'react'

export type CommonErrorPageProps = {
  title: number
  description: string | ReactNode
  titleClassName: string
  color: string
}

export type CommonCardHeadingPropsType = {
  bigHeadingClassName?: string
  Heading?: string
  smallHeading?: string
  span?: string
  headingClassName?: string
  span2?: string
}

export type Upload = {
  filename: string
  mimetype: string
  encoding: string
}

export type PaginationMeta = {
  total: number
  lastPage: number
  currentPage: number
  perPage: number
  prev: number | null
  next: number | null
}

export type PaginatedObject<T> = {
  data: T[]
  meta: PaginationMeta
}

export type WhereAndOrderInput = {
  where?: FindManyInput
  orderBy?: OrderByInput
}

export type WhereInput = {
  where?: FindManyInput
}

export type OrderInput = {
  orderBy?: OrderByInput
}

export type FindManyInput = {
  [key: string]: any
}

export type OrderByInput = {
  [key: string]: 'ASC' | 'DESC' | OrderByEnum | string
}

export type PaginatedInput = {
  page: number
  perPage: number
  where?: FindManyInput
  orderBy?: OrderByInput
}

export type FindBySlugInput = {
  slug: string
}

export type FindByIdInput = {
  id: number
}

export type FindByIdsInput = {
  ids: number[]
}

type ParamsType = {
  id?: number
  page?: string | number
  slug?: string
  subdomain?: string
  locale: string
}

export type ErrorPageProps = {
  error: Error & { digest?: string }
  reset: () => void
}

export type ChildrenProps = {
  children: ReactNode
}
export type PageParamsProps = {
  params: ParamsType
}

export type ChildrenWithParamsProps = {} & PageParamsProps & ChildrenProps

export type ReduxProviderProps = {
  host: string
} & ChildrenProps

export type AuthFormProps = {
  alignLogo?: string
}

export type PageWithChildrenProps = {
  page: number
  children?: ReactNode
}

export enum ModeEnum {
  SENSITIVE = 'SENSITIVE',
  INSENSITIVE = 'INSENSITIVE',
}

export enum OrderByEnum {
  ASC = 'ASC',
  DESC = 'DESC',
}

export type CardContextType = {
  cardTitle: string
  setCardTitle: (title: string) => void
  cardDescription: string
  setCardDescription: (description: string) => void
}

export type ReactSelectType = {
  label: string
  value: string | number
}
