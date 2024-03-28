import Image from 'next/image'
import React, { useEffect, useState } from 'react'

import { useAuth } from '@/hooks'
import { Account } from '@/types/account'

export const Trading = () => {
  const [trading, setTrading] = useState<Account | null>(null)

  const { jwt } = useAuth()

  useEffect(() => {
    if (jwt?.account) setTrading(jwt.account)
  }, [jwt])

  return (
    <>
      <div className="d-flex h-100 align-items-center">
        {trading && trading?.tradingLogo && (
          <Image
            src={trading.tradingLogo}
            alt={trading.systemName || trading.tradingName || 'Company Logo'}
            width={36}
            height={36}
            className="img-fluid me-3"
          />
        )}
        {trading && (
          <h4 className="mb-0 f-w-600">
            <span>{trading.systemName || trading.tradingName}</span>
          </h4>
        )}
      </div>
    </>
  )
}
