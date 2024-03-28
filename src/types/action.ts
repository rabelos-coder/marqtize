export enum ActionEnum {
  Create = 'Create',
  Read = 'Read',
  Update = 'Update',
  Delete = 'Delete',
  Manage = 'Manage',
}

export type Action = keyof typeof ActionEnum
