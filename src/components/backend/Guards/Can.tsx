import { AnyAbility } from "@casl/ability";
import { BoundCanProps, createContextualCan } from "@casl/react";

import { AbilityContext } from "@/context/AbilityContext";

/**
 * Can component to provide ability context to its children.
 *
 * @param {string} do - The ability to check.
 * @param {string} I - The subject to check.
 * @param {BoundCanProps<AnyAbility>} children - The child components to provide ability context to.
 * @return {JSX.Element} The ability context provided to the children.
 */
export const Can: React.FC<BoundCanProps<AnyAbility>> = createContextualCan(
  AbilityContext.Consumer
);
