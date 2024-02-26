import { createAsyncThunk,createSlice } from "@reduxjs/toolkit";
import secureLocalStorage from "react-secure-storage";

import { STORAGE_CUSTOMER } from "@/configs";
import { GET_CUSTOMER_BY_SLUG } from "@/graphql/customer";
import { CustomerState } from "@/types/customer";
import { createApolloClient } from "@/utils/apollo";

let customer: any =
  typeof window !== "undefined"
    ? (secureLocalStorage.getItem(STORAGE_CUSTOMER) as string) ?? null
    : null;

if (customer) {
  customer = JSON.parse(customer);
}

const initialState: CustomerState = {
  customer,
  loading: false,
  error: null,
};

export const fetchCustomer = createAsyncThunk(
  "customer/fetchCustomer",
  async (host: string) => {
    const client = createApolloClient();

    try {
      const { data, errors } = await client.mutate({
        mutation: GET_CUSTOMER_BY_SLUG,
        variables: {
          slug: host.replace(
            "." + (process.env.NEXT_PUBLIC_ROOT_DOMAIN ?? "localhost:3000"),
            ""
          ),
        },
      });

      if (!data && errors?.length) throw new Error(errors[0].message);

      if (data) {
        const { findBySlugCustomer } = data;

        return findBySlugCustomer;
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }
);

export const customerSlice = createSlice({
  name: "customer",
  initialState,
  reducers: {
    resetCustomer: (state) => {
      state.customer = initialState.customer;
      if (typeof window !== "undefined") {
        secureLocalStorage.removeItem(STORAGE_CUSTOMER);
      }
    },
    setCustomer: (state, action) => {
      state.customer = action.payload;
      if (typeof window !== "undefined") {
        secureLocalStorage.setItem(
          STORAGE_CUSTOMER,
          JSON.stringify(state.customer)
        );
      }
    },
  },
  extraReducers: (builder) => {
    builder.addCase(fetchCustomer.pending, (state) => {
      state.loading = true;
      state.error = null;
    });
    builder.addCase(fetchCustomer.rejected, (state, action) => {
      state.loading = false;
      state.error = `${action.error.message}`;
    });
    builder.addCase(fetchCustomer.fulfilled, (state, action) => {
      state.loading = false;
      state.error = null;
      state.customer = action.payload ?? null;
      if (typeof window !== "undefined") {
        if (state.customer)
          secureLocalStorage.setItem(
            STORAGE_CUSTOMER,
            JSON.stringify(state.customer)
          );
      }
    });
  },
});

export const { setCustomer, resetCustomer } = customerSlice.actions;
export default customerSlice.reducer;
