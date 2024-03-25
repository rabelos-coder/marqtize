import { createAsyncThunk, createSlice, PayloadAction } from '@reduxjs/toolkit'
import Cookies from 'js-cookie'

import { STORAGE_CUSTOMER } from '@/configs'
import { FIND_CUSTOMER_BY_SLUG } from '@/graphql/customer'
import {
  Customer,
  CustomerState,
  FindBySlugOrHostInput,
} from '@/types/customer'
import { createApolloClient } from '@/utils/apollo'

let customer: any = Cookies.get(STORAGE_CUSTOMER) ?? null

try {
  if (customer) {
    customer = JSON.parse(customer)
  }
} catch {
  customer = null
}

const initialState: CustomerState = {
  customer,
  loading: false,
  error: null,
}

export const fetchCustomer = createAsyncThunk(
  'customer/paths[2]',
  async (input: FindBySlugOrHostInput) => {
    const client = createApolloClient()
    const { slug, host } = input

    if (!slug && !host) return null

    try {
      const { data, errors } = await client.mutate({
        mutation: FIND_CUSTOMER_BY_SLUG,
        variables: { slug, host },
      })

      if (!data && errors?.length) throw new Error(errors[0].message)

      if (data) {
        const { findBySlugOrHostCustomer } = data

        return findBySlugOrHostCustomer
      }
    } catch (error) {
      throw error
    }

    return null
  }
)

export const customerSlice = createSlice({
  name: 'customer',
  initialState,
  reducers: {
    resetCustomer: (state) => {
      state.loading = false
      state.error = null
      state.customer = initialState.customer
      Cookies.remove(STORAGE_CUSTOMER)
    },
    resetCustomerError: (state) => {
      state.loading = false
      state.error = null
    },
    setCustomer: (state, action: PayloadAction<Customer>) => {
      state.loading = false
      state.error = null
      state.customer = action.payload
      Cookies.set(STORAGE_CUSTOMER, JSON.stringify(state.customer))
    },
  },
  extraReducers: (builder) => {
    builder.addCase(fetchCustomer.pending, (state) => {
      state.loading = true
      state.error = null
    })
    builder.addCase(fetchCustomer.rejected, (state, action) => {
      state.loading = false
      state.error = `${action.error.message}`
      state.customer = null
      Cookies.remove(STORAGE_CUSTOMER)
    })
    builder.addCase(fetchCustomer.fulfilled, (state, action) => {
      state.loading = false
      state.error = null
      state.customer = action.payload ?? null

      if (!!state.customer)
        Cookies.set(STORAGE_CUSTOMER, JSON.stringify(state.customer))
      else Cookies.remove(STORAGE_CUSTOMER)
    })
  },
})

export const { setCustomer, resetCustomerError, resetCustomer } =
  customerSlice.actions
export default customerSlice.reducer
