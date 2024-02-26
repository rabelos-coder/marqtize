import { gql,TypedDocumentNode } from "@apollo/client";

import {
  AuthLogin,
  LoginInput,
  Register,
  RegisterInput,
  User,
} from "@/types/auth";

const USER_PROPS = gql`
  fragment UserProps on User {
    id
    name
    systemName
    email
    image
    isActive
    isSuperAdmin
    language
    timezoneId
    type
    claims
    timezone {
      id
      code
      name
    }
    customer {
      id
      corporateName
      tradingName
      systemName
      slug
    }
  }
`;

export const AUTH_LOGIN: TypedDocumentNode<AuthLogin, LoginInput> = gql`
  ${USER_PROPS}
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
  ${USER_PROPS}
  mutation Register($data: RegisterInput!) {
    register(data: $data) {
      ...UserProps
    }
  }
`;

type NextAuthLogin = {
  nextAuthLogin: {
    token: string;
    user: User;
  };
};

type Email = {
  email: string;
};

export const NEXT_AUTH_LOGIN: TypedDocumentNode<NextAuthLogin, Email> = gql`
  mutation nextAuthLogin($email: String!) {
    nextAuthLogin(email: $email) {
      token
      user {
        ...UserProps
      }
    }
  }
`;

export const GET_USER_AND_SESSION_BY_USER_ID = gql`
  ${USER_PROPS}
  query FindSessionAndUserByUserId($userId: String!) {
    findSessionAndUserByUserId(userId: $userId) {
      user {
        ...UserProps
      }
      session {
        id
        sessionToken
        userId
        expires
        createdAt
        updatedAt
      }
    }
  }
`;

export const FIND_USER_BY_ID = gql`
  ${USER_PROPS}
  query FindUserById($id: String!) {
    findUserById(id: $id) {
      ...UserProps
    }
  }
`;

export const FIND_USER_BY_EMAIL = gql`
  ${USER_PROPS}
  query FindUserByEmail($email: String!) {
    findUserByEmail(email: $email) {
      ...UserProps
    }
  }
`;

export const FIND_USER_BY_ACCOUNT = gql`
  ${USER_PROPS}
  query FindUserByAccount($data: SearchUserByAccount!) {
    findUserByAccount(data: $data) {
      ...UserProps
    }
  }
`;

export const GET_SESSION_AND_USER = gql`
  ${USER_PROPS}
  query FindSessionAndUser($sessionToken: String!) {
    findSessionAndUser(sessionToken: $sessionToken) {
      user {
        ...UserProps
      }
      session {
        id
        sessionToken
        userId
        expires
        createdAt
        updatedAt
      }
    }
  }
`;

export const LINK_USER_ACCOUNT = gql`
  ${USER_PROPS}
  mutation LinkUserAccount($data: CreateAccountInput!) {
    linkUserAccount(data: $data) {
      id
      userId
      type
      provider
      providerAccountId
      refresh_token
      access_token
      expires_at
      token_type
      scope
      id_token
      session_state
      user {
        ...UserProps
      }
    }
  }
`;

export const UNLINK_USER_ACCOUNT = gql`
  ${USER_PROPS}
  mutation UnlinkUserAccount($data: SearchUserByAccount!) {
    unlinkUserAccount(data: $data) {
      id
      userId
      type
      provider
      providerAccountId
      refresh_token
      access_token
      expires_at
      token_type
      scope
      id_token
      session_state
      user {
        ...UserProps
      }
    }
  }
`;

export const CREATE_SESSION = gql`
  ${USER_PROPS}
  mutation CreateSession($data: CreateSessionInput!) {
    createSession(data: $data) {
      id
      sessionToken
      userId
      expires
      user {
        ...UserProps
      }
    }
  }
`;

export const UPDATE_SESSION = gql`
  ${USER_PROPS}
  mutation UpdateSession($data: UpdateSessionInput!) {
    updateSession(data: $data) {
      id
      sessionToken
      userId
      expires
      createdAt
      updatedAt
      user {
        ...UserProps
      }
    }
  }
`;

export const DELETE_SESSION = gql`
  ${USER_PROPS}
  mutation DeleteSession($sessionToken: String!) {
    deleteSession(sessionToken: $sessionToken) {
      id
      sessionToken
      userId
      expires
      createdAt
      updatedAt
      user {
        ...UserProps
      }
    }
  }
`;

export const CREATE_VERIFICATION_TOKEN = gql`
  mutation CreateVerificationToken($data: CreateVerificationTokenInput!) {
    createVerificationToken(data: $data) {
      id
      identifier
      token
      expires
    }
  }
`;

export const USE_VERIFICATION_TOKEN = gql`
  mutation UseVerificationToken($data: SearchIdentifierTokenInput!) {
    useVerificationToken(data: $data) {
      id
      identifier
      token
      expires
    }
  }
`;

export const CREATE_SOCIAL_LOGIN = gql`
  ${USER_PROPS}
  mutation CreateSocialLogin($data: CreateSocialLoginInput!) {
    createSocialLogin(data: $data) {
      ...UserProps
    }
  }
`;

export const UPDATE_SOCIAL_LOGIN = gql`
  ${USER_PROPS}
  mutation UpdateSocialLogin($data: UpdateSocialLoginInput!) {
    updateSocialLogin(data: $data) {
      ...UserProps
    }
  }
`;

export const REGISTER_USER = gql`
  ${USER_PROPS}
  mutation Register($data: RegisterInput!) {
    register(data: $data) {
      ...UserProps
    }
  }
`;

export const FORGOT_PASSWORD = gql`
  mutation ForgotPassword($data: ForgotPasswordInput!) {
    forgotPassword(data: $data)
  }
`;

export const RESET_PASSWORD = gql`
  mutation ResetPassword($data: ResetPasswordInput!) {
    resetPassword(data: $data)
  }
`;
