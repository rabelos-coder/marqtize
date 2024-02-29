import { APP_META_TITLE, APP_META_TITLE_SEPARATOR } from "@/environment";

/**
 * Concatenates the given title with the global application meta title and separator.
 *
 * @param {string} title - The title to be concatenated
 * @return {string} The concatenated title
 */
export const concatTitle = (title: string): string =>
  `${APP_META_TITLE} ${APP_META_TITLE_SEPARATOR} ${title}`;
