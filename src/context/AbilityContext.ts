'use client'

import { AnyAbility } from '@casl/ability'
import { createContext } from 'react'

export const AbilityContext = createContext<AnyAbility>(undefined!)
