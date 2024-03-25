export enum UserTypeEnum {
  CREDENTIAL = 'CREDENTIAL',
  APPLICATION = 'APPLICATION',
}

export type UserType = keyof typeof UserTypeEnum
