import { createAsyncThunk, createSlice, PayloadAction } from '@reduxjs/toolkit'
import Cookies from 'js-cookie'

import { STORAGE_ACCOUNT } from '@/configs'
import { FIND_ACCOUNT_BY_SLUG } from '@/graphql/account'
import { Account, AccountState, FindBySlugOrHostInput } from '@/types/account'
import { createApolloClient } from '@/utils/apollo'

let account: any = Cookies.get(STORAGE_ACCOUNT) ?? null

try {
  if (account) {
    account = JSON.parse(account)
  }
} catch {
  account = null
}

const initialState: AccountState = {
  account,
  loading: false,
  error: null,
}

export const fetchAccount = createAsyncThunk(
  'account/account',
  async (input: FindBySlugOrHostInput) => {
    const client = createApolloClient()
    const { slug, host } = input

    if (!slug && !host) return null

    try {
      const { data, errors } = await client.mutate({
        mutation: FIND_ACCOUNT_BY_SLUG,
        variables: { slug, host },
      })

      if (!data && errors?.length) throw new Error(errors[0].message)

      if (data) {
        const { findBySlugOrHostAccount } = data

        return findBySlugOrHostAccount
      }
    } catch (error) {
      throw error
    }

    return null
  }
)

export const accountSlice = createSlice({
  name: 'account',
  initialState,
  reducers: {
    resetAccount: (state) => {
      state.loading = false
      state.error = null
      state.account = initialState.account
      Cookies.remove(STORAGE_ACCOUNT)
    },
    resetAccountError: (state) => {
      state.loading = false
      state.error = null
    },
    setAccount: (state, action: PayloadAction<Account>) => {
      state.loading = false
      state.error = null
      state.account = action.payload
      Cookies.set(STORAGE_ACCOUNT, JSON.stringify(state.account))
    },
  },
  extraReducers: (builder) => {
    builder.addCase(fetchAccount.pending, (state) => {
      state.loading = true
      state.error = null
    })
    builder.addCase(fetchAccount.rejected, (state, action) => {
      state.loading = false
      state.error = `${action.error.message}`
      state.account = null
      Cookies.remove(STORAGE_ACCOUNT)
    })
    builder.addCase(fetchAccount.fulfilled, (state, action) => {
      state.loading = false
      state.error = null
      state.account = action.payload ?? null

      if (!!state.account)
        Cookies.set(STORAGE_ACCOUNT, JSON.stringify(state.account))
      else Cookies.remove(STORAGE_ACCOUNT)
    })
  },
})

export const { setAccount, resetAccountError, resetAccount } =
  accountSlice.actions
export default accountSlice.reducer
