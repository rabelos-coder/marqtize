import { Upload } from './common'
import { UserTypeEnum } from './enums'
import { JWT } from './jwt'
import { User } from './user'

export type AuthState = {
  user: User | null
  token: string | null
  jwt: JWT | null
  language: string
  timezone: string
  isLoggedIn: boolean
}

export type AuthContextType = {
  user: User | null
  token: string | null
  language: string
  timezone: string
  logout: () => void
  isLoggedIn: boolean
}

export type WhoAmI = {
  whoAmI: User
}

export type UpdateProfile = {
  updateProfile: User
}

export type UpdateProfileInput = {
  data: {
    name?: string | null
    systemName?: string | null
    email?: string | null
    password?: string | null
    language?: string | null
    timezoneId?: string | null
    removeImage?: boolean | null
    imageFile?: File | null
  }
}

export type Register = {
  register: User
}

export type Auth = {
  token: string
  user: User
}

export type Login = {
  login: Auth
}

export type ForgotPassword = {
  forgotPassword: boolean
}

export type ResetPassword = {
  resetPassword: boolean
}

export type LoginInput = {
  data: {
    email: string
    password: string
    rememberMe: boolean
  }
}

export type RegisterInput = {
  data: {
    customerId?: string | null
    name: string
    systemName: string
    email: string
    password: string
    imageFile?: Upload
    language?: string
    type?: UserTypeEnum
    timezoneId?: string
  }
}

export type ForgotPasswordInput = {
  data: {
    email: string
    callbackUrl: string
  }
}

export type ResetPasswordInput = {
  data: {
    email: string
    password: string
    resetToken: string
  }
}

export type ProfileProps = {
  user: User
}

export type EditProfileProps = {
  timezones: Array<{ label: string; value: string }>
  languages: Array<{ label: string; value: string }>
} & ProfileProps
