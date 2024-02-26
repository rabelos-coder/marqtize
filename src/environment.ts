// * ENVIRONMENT SETTINGS
export const ENVIRONMENT = process.env.NODE_ENV || "development";
export const IS_PRODUCTION = ENVIRONMENT === "production";
export const IS_DEVELOPMENT = ENVIRONMENT === "development";

// * APPLICATION SETTINGS
export const APP_VERSION = process.env.npm_package_version || "1.0.0";
export const APP_LANGUAGE = process.env.NEXT_PUBLIC_APP_LANGUAGE || "pt";
export const APP_TIMEZONE =
  process.env.NEXT_PUBLIC_APP_TIMEZONE || "America/Sao_Paulo";
export const APP_DATETIME_FORMAT =
  process.env.NEXT_PUBLIC_APP_DATETIME_FORMAT || "lll";
export const APP_META_TITLE =
  process.env.NEXT_PUBLIC_APP_META_TITLE || "NextJS";
export const APP_META_TITLE_SEPARATOR =
  process.env.NEXT_PUBLIC_APP_META_TITLE_SEPARATOR || "|";
export const APP_META_KEYWORDS =
  process.env.NEXT_PUBLIC_APP_META_KEYWORDS || "";
export const APP_META_DESCRIPTION =
  process.env.NEXT_PUBLIC_APP_META_DESCRIPTION || "";

// * SERVER SETTINGS
export const SERVER_URL =
  process.env.NEXT_PUBLIC_SERVER_URL ?? "http://localhost:4000";

// * IMAGE UPLOAD SETTINGS
export const IMAGE_UPLOAD_MAX_WIDTH = parseInt(
  process.env.NEXT_PUBLIC_IMAGE_UPLOAD_MAX_WIDTH ?? "800"
);
export const IMAGE_UPLOAD_MAX_QUALITY = parseInt(
  process.env.NEXT_PUBLIC_IMAGE_UPLOAD_MAX_QUALITY ?? "90"
);

// * THEME SETTINGS
export const THEME_LOCALE_SWITCHER_ENABLED =
  process.env.NEXT_PUBLIC_THEME_LOCALE_SWITCHER_ENABLED === "true";

export const THEME_CUSTOMIZER_ENABLED =
  process.env.NEXT_PUBLIC_THEME_CUSTOMIZER_ENABLED === "true";
