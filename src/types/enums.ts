export enum UserTypeEnum {
  credentials = "credentials",
  application = "application",
}

export type UserType = keyof typeof UserTypeEnum;
