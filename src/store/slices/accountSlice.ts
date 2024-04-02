import { createAsyncThunk, createSlice, PayloadAction } from '@reduxjs/toolkit'
import axios from 'axios'
import Cookies from 'js-cookie'

import { STORAGE_ACCOUNT } from '@/configs'
import { APP_MAIN_DOMAIN } from '@/environment'
import { Account, AccountState, FindBySlugOrHostInput } from '@/types/account'

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
  async (input: FindBySlugOrHostInput): Promise<Account | null> => {
    const { slug, host } = input

    if (host === APP_MAIN_DOMAIN) return null

    if (!slug && !host) return null

    return await axios
      .post<Account>('/api/account', { slug, host })
      .then(({ data }) => data ?? null)
      .catch((error) => {
        console.log(error?.response?.data?.message ?? 'No account found')

        return null
      })
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
