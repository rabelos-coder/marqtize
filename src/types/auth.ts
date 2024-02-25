import { Timezone } from "./timezone";

export type AuthContextType = {
  user: User | null;
  token: string | null;
  language: string | null;
  timezone: string | null;
  login: (input: LoginInput) => Promise<void>;
  logout: () => void;
  register: (input: RegisterInput) => Promise<void>;
};

export type User = {
  id: number;
  email: string;
  name: string;
  image: string;
  role: string;
  language: string;
  timezone: Timezone;
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
