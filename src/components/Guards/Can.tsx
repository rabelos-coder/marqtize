import { AbilityContext } from "@/contexts/AbilityContext";
import { AnyAbility } from "@casl/ability";
import { BoundCanProps, createContextualCan } from "@casl/react";

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
