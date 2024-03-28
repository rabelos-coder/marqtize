import { Account } from './account'
import { UserType } from './enums'
import { Language } from './language'

export type JWT = {
  id: string
  tokenId: string | null
  sa: boolean
  systemName: string
  account: Account | null
  email: string
  roles: string[]
  claims: string[]
  type: UserType
  lang: Language
  image: string | null
  iat: number
  exp: number
}
