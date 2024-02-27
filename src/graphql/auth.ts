import { gql, TypedDocumentNode } from "@apollo/client";

import {
  AuthLogin,
  ForgotPassword,
  ForgotPasswordInput,
  LoginInput,
  Register,
  RegisterInput,
  ResetPassword,
  ResetPasswordInput,
} from "@/types/auth";

import { FRAGMENT_USER_PROPS } from "./users";

export const AUTH_LOGIN: TypedDocumentNode<AuthLogin, LoginInput> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation AuthLogin($data: LoginInput!) {
    authLogin(data: $data) {
      token
      user {
        ...UserProps
      }
    }
  }
`;

export const REGISTER: TypedDocumentNode<Register, RegisterInput> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation Register($data: RegisterInput!) {
    register(data: $data) {
      ...UserProps
    }
  }
`;

export const FORGOT_PASSWORD: TypedDocumentNode<
  ForgotPassword,
  ForgotPasswordInput
> = gql`
  mutation ForgotPassword($data: ForgotPasswordInput!) {
    forgotPassword(data: $data)
  }
`;

export const RESET_PASSWORD: TypedDocumentNode<
  ResetPassword,
  ResetPasswordInput
> = gql`
  mutation ResetPassword($data: ResetPasswordInput!) {
    resetPassword(data: $data)
  }
`;
