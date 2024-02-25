import { AbilityContext } from "@/contexts/AbilityContext";
import { AuthContext } from "@/contexts/AuthContext";
import { AuthContextType } from "@/types/auth";
import { AnyAbility } from "@casl/ability";
import { useContext } from "react";

/**
 * Returns the authentication context.
 *
 * @return {AuthContextType} the authentication context
 */
export const useAuth = (): AuthContextType => useContext(AuthContext);

/**
 * Returns the ability context.
 *
 * @returns {AnyAbility} the ability context
 */
export const useAbility = (): AnyAbility => useContext(AbilityContext);
