export type CustomerState = {
  customer: Customer | null;
  loading: boolean;
  error: string | null;
};

export type Customer = {
  id?: string;
  userId?: string | null;
  erpId?: string | null;
  corporateNumber?: string | null;
  corporateName: string;
  tradingName?: string | null;
  systemName: string;
  email: string;
  slug: string;
  phone?: string | null;
  mobile?: string | null;
  postcode?: string | null;
  address?: string | null;
  number?: string | null;
  complement?: string | null;
  neighborhood?: string | null;
  countryId?: string | null;
  cityId?: string | null;
  stateId?: string | null;
  isActive?: boolean;
  tradingLogo?: string | null;
  createdAt?: Date | string;
  updatedAt?: Date | string;
  deletedAt?: Date | string | null;
};
