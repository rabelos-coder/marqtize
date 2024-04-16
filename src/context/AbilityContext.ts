'use client'

import { AnyAbility } from '@casl/ability'
import { createContext } from 'react'

const AbilityContext = createContext<AnyAbility>(undefined!)

export default AbilityContext
