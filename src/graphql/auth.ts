import { gql, TypedDocumentNode } from '@apollo/client'

import {
  ForgotPassword,
  ForgotPasswordInput,
  Login,
  LoginInput,
  Register,
  RegisterInput,
  ResetPassword,
  ResetPasswordInput,
  UpdateProfile,
  UpdateProfileInput,
  WhoAmI,
} from '@/types/auth'

import { FRAGMENT_USER_PROPS } from './users'

export const WHO_AM_I: TypedDocumentNode<WhoAmI> = gql`
  ${FRAGMENT_USER_PROPS}
  query WhoAmI {
    whoAmI {
      ...UserProps
    }
  }
`

export const LOGIN: TypedDocumentNode<Login, LoginInput> = gql`
  mutation Login($data: LoginInput!) {
    login(data: $data) {
      token
      user {
        name
        systemName
        image
        language
        email
        timezone {
          code
        }
        roles {
          name
        }
        account {
          systemName
          tradingName
          corporateName
          tradingLogo
        }
      }
    }
  }
`

export const REGISTER: TypedDocumentNode<Register, RegisterInput> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation Register($data: RegisterInput!) {
    register(data: $data) {
      ...UserProps
    }
  }
`

export const FORGOT_PASSWORD: TypedDocumentNode<
  ForgotPassword,
  ForgotPasswordInput
> = gql`
  mutation ForgotPassword($data: ForgotPasswordInput!) {
    forgotPassword(data: $data)
  }
`

export const RESET_PASSWORD: TypedDocumentNode<
  ResetPassword,
  ResetPasswordInput
> = gql`
  mutation ResetPassword($data: ResetPasswordInput!) {
    resetPassword(data: $data)
  }
`

export const UPDATE_PROFILE: TypedDocumentNode<
  UpdateProfile,
  UpdateProfileInput
> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation UpdateProfile($data: UpdateProfileInput!) {
    updateProfile(data: $data) {
      ...UserProps
    }
  }
`
