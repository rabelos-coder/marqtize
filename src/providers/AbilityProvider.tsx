'use client'

import AbilityContext from '@/context/AbilityContext'
import { AbilityProviderProps } from '@/types/ability'

/**
 * AbilityProvider component that provides abilities to its children.
 *
 * @param {AbilityProviderProps} children - The children components
 * @param {AnyAbility[]} abilities - The abilities to be provided
 * @return {JSX.Element} The component with provided abilities
 */
export default function AbilityProvider({
  children,
  ability,
}: AbilityProviderProps): JSX.Element {
  return (
    <AbilityContext.Provider value={ability}>
      {children}
    </AbilityContext.Provider>
  )
}
