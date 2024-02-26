import { AnyAbility } from "@casl/ability";
import { useContext } from "react";
import type { TypedUseSelectorHook } from "react-redux";
import { useDispatch, useSelector } from "react-redux";

import { AbilityContext } from "@/contexts/AbilityContext";
import { AuthContext } from "@/contexts/AuthContext";
import { CustomizerContext } from "@/contexts/CustomizerContext";
import { LayoutContext } from "@/contexts/LayoutContext";
import { AuthContextType } from "@/types/auth";
import { CustomizerContextType } from "@/types/customizer";
import { LayoutContextType } from "@/types/layout";

import type { AppDispatch,RootState } from "../store";

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

/**
 * Returns the Redux dispatch function.
 *
 * @return {AppDispatch} The Redux dispatch function
 */
export const useAppDispatch: () => AppDispatch = useDispatch;

/**
 * Returns the Redux state selector.
 *
 * @return {TypedUseSelectorHook<RootState>} The Redux root state function
 */
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
