import { Upload } from "./common";
import { UserTypeEnum } from "./enums";
import { JWT } from "./jwt";
import { User } from "./user";

export type AuthState = {
  user: User | null;
  token: string | null;
  jwt: JWT | null;
  language: string;
  timezone: string;
  loading: boolean;
  error: string | null;
};

export type AuthContextType = {
  user: User | null;
  token: string | null;
  language: string;
  timezone: string;
  loading: boolean;
  error: string | null;
  login: (input: LoginInput) => Promise<void>;
  logout: () => void;
  register: (input: RegisterInput) => Promise<void>;
  isLoggedIn: boolean;
};

export type Register = {
  register: User;
};

export type ForgotPassword = {
  authLogin: {
    token: string;
    user: User;
  };
};

export type AuthLogin = {
  authLogin: {
    token: string;
    user: User;
  };
};

export type ResetPassword = {
  resetPassword: boolean;
};

export type LoginInput = {
  data: {
    email: string;
    password: string;
    rememberMe: boolean;
  };
};

export type RegisterInput = {
  data: {
    customerId?: string;
    name: string;
    systemName: string;
    email: string;
    password: string;
    imageFile?: Upload;
    language: string;
    type: UserTypeEnum;
    timezoneId: string;
  };
};

export type ForgotPasswordInput = {
  data: {
    email: string;
    callbackUrl: string;
  };
};

export type ResetPasswordInput = {
  data: {
    email: string;
    password: string;
    resetToken: string;
  };
};
