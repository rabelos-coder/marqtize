import { gql } from "@apollo/client";

export const GET_CUSTOMER = gql`
  query FindFirstCustomer(
    $where: SearchCustomerInput
    $orderBy: SortCustomerInput
  ) {
    findFirstCustomer(where: $where, orderBy: $orderBy) {
      id
      createdAt
      updatedAt
      deletedAt
      userId
      erpId
      corporateNumber
      corporateName
      tradingName
      systemName
      email
      slug
      phone
      mobile
      postcode
      address
      number
      complement
      neighborhood
      countryId
      stateId
      cityId
      isActive
      tradingLogo
      country {
        name
      }
      state {
        name
      }
      city {
        name
      }
    }
  }
`;

export const GET_CUSTOMER_BY_SLUG = gql`
  query FindBySlugCustomer($slug: String!) {
    findBySlugCustomer(slug: $slug) {
      id
      createdAt
      updatedAt
      deletedAt
      userId
      erpId
      corporateNumber
      corporateName
      tradingName
      systemName
      email
      slug
      phone
      mobile
      postcode
      address
      number
      complement
      neighborhood
      countryId
      stateId
      cityId
      isActive
      tradingLogo
      country {
        name
      }
      state {
        name
      }
      city {
        name
      }
    }
  }
`;
