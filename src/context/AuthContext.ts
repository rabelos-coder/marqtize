'use client'

import { createContext } from 'react'

import { AuthContextType } from '@/types/auth'

const AuthContext = createContext<AuthContextType>(undefined!)

export default AuthContext
