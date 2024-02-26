import { Customer } from "./customer";
import { UserTypeEnum } from "./enums";
import { JWT } from "./jwt";
import { Role } from "./role";
import { Timezone } from "./timezone";

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

export type User = {
  id: string;
  customerId: string;
  name: string;
  systemName: string;
  email: string;
  emailVerified: string;
  password: string;
  isActive: boolean;
  isSuperAdmin: boolean;
  image: string;
  language: string;
  type: UserTypeEnum;
  resetToken: string;
  resetTokenExpires: string;
  twoFactorSecret: string;
  twoFactorRecoveryCodes: string;
  twoFactorConfirmedAt: string;
  timezoneId: string;
  createdAt: string;
  updatedAt: string;
  deletedAt: string;

  roles: Role[];
  timezone: Timezone;
  customer: Customer;
};

export type Register = {
  register: User;
};

export type RegisterInput = {
  data: any;
};

export type AuthLogin = {
  authLogin: {
    token: string;
    user: User;
  };
};

export type LoginInput = {
  data: {
    email: string;
    password: string;
    rememberMe: boolean;
  };
};
