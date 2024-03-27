'use client'

import { useRef } from 'react'
import { Provider } from 'react-redux'

import { APP_MAIN_DOMAIN } from '@/environment'
import { usePathname } from '@/navigation'
import { fetchAccount } from '@/store/slices/accountSlice'
import { ReduxProviderProps } from '@/types/common'

import { AppStore, makeStore } from '../store'

export function ReduxProvider({ host, children }: ReduxProviderProps) {
  const storeRef = useRef<AppStore | null>(null)
  const pathname = usePathname()

  if (!storeRef.current) {
    let slug = host.replace(`.${APP_MAIN_DOMAIN}`, '')

    if (pathname.includes('sub-domains')) {
      const paths = pathname.split('/')
      if (paths.length > 2) {
        slug = paths[2]
        host = `${paths[2]}.localhost`
      }
    }

    // Create the store instance the first time this renders
    storeRef.current = makeStore()
    storeRef.current.dispatch(fetchAccount({ slug, host }))
  }

  return <Provider store={storeRef.current}>{children}</Provider>
}
