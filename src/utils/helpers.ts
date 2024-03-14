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

/**
 * Retrieves the valid subdomain from the given host.
 *
 * @param {string | null} host - The host from which to retrieve the subdomain.
 * @return {string | null} The valid subdomain, if found; otherwise, null.
 */
export const getValidSubdomain = (host?: string | null) => {
  let subdomain: string | null = null;
  if (!host && typeof window !== "undefined") {
    // On client side, get the host from window
    host = window.location.host;
  }
  if (host && host.includes(".")) {
    const candidate = host.split(".")[0];
    if (candidate && !candidate.includes("localhost")) {
      // Valid candidate
      subdomain = candidate;
    }
  }

  return subdomain;
};

/**
 * Handles click event on an anchor element, prevents default behavior, and scrolls to the referenced element.
 *
 * @param {React.MouseEvent<HTMLAnchorElement>} e - The click event object
 * @return {void}
 */
export const anchorClick = (e: React.MouseEvent<HTMLAnchorElement>): void => {
  if (typeof e === "object") {
    e?.preventDefault();
    document
      ?.querySelector(`${e?.currentTarget?.getAttribute("href")}`)
      ?.scrollIntoView({
        behavior: "smooth",
      });
  }
};
