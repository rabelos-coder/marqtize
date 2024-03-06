export type Role = {
  id: string;
  name: string;
  slug: string;
  claims: string[];
  createdAt: Date;
  updatedAt: Date;
};

export type CountRole = {
  countRole: number;
};

export type FindManyRole = {
  findManyRole: Role[];
};

export type FindByIdRole = {
  findByIdRole: Role;
};

export type FindFirstRole = {
  findFirstRole: Role;
};

export type CreateRole = {
  createRole: Role;
};

export type UpdateRole = {
  updateRole: Role;
};

export type DeleteRole = {
  deleteRole: boolean;
};

export type DeleteManyRole = {
  deleteManyRole: boolean;
};

export type RemoveRole = {
  removeRole: boolean;
};

export type RemoveManyRole = {
  removeManyRole: boolean;
};

export type RestoreRole = {
  restoreRole: boolean;
};

export type RestoreManyRole = {
  restoreManyRole: boolean;
};

type RoleInput = {
  customerId?: string;
  name: string;
  isDefault: boolean;
  claims?: string[];
};

export type CreateRoleInput = {
  data: RoleInput;
};

type RoleUpdateInput = {
  id: string;
} & RoleInput;

export type UpdateRoleInput = {
  data: RoleUpdateInput;
};
