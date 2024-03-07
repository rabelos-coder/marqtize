import { Metadata } from "next/types";

import { APP_META_TITLE, APP_META_TITLE_SEPARATOR } from "@/environment";

/**
 * Concatenates the given title with the global application meta title and separator.
 *
 * @param {string} title - The title to be concatenated
 * @return {string} The concatenated title
 */
export const concatTitle = (title: string): string =>
  `${APP_META_TITLE} ${APP_META_TITLE_SEPARATOR} ${title}`;

export const icons: Metadata["icons"] = [
  {
    url: "/assets/images/favicon/apple-touch-icon.png",
    rel: "apple-touch-icon",
    sizes: "180x180",
  },
  {
    url: "/assets/images/favicon/favicon-32x32.png",
    rel: "icon",
    sizes: "32x32",
    type: "image/png",
  },
  {
    url: "/assets/images/favicon/favicon-16x16.png",
    rel: "icon",
    sizes: "16x16",
    type: "image/png",
  },
];
