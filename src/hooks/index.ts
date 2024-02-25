import { AbilityContext } from "@/contexts/AbilityContext";
import { AuthContext } from "@/contexts/AuthContext";
import { CustomizerContext } from "@/contexts/CustomizerContext";
import { LayoutContext } from "@/contexts/LayoutContext";
import { AuthContextType } from "@/types/auth";
import { CustomizerContextType } from "@/types/customizer";
import { LayoutContextType } from "@/types/layout";
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

/**
 * Returns the Customizer context type by using the useContext hook.
 *
 * @return {CustomizerContextType} The Customizer context type
 */
export const useCustomizer = (): CustomizerContextType =>
  useContext(CustomizerContext);

/**
 * Returns the Layout context type by using the useContext hook.
 *
 * @return {LayoutContextType} The Layout context type
 */
export const useLayout = (): LayoutContextType => useContext(LayoutContext);
