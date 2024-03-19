// * ENVIRONMENT SETTINGS
export const ENVIRONMENT = process.env.NODE_ENV ?? "development";
export const IS_PRODUCTION = ENVIRONMENT === "production";
export const IS_DEVELOPMENT = ENVIRONMENT === "development";

// * APPLICATION SETTINGS
export const APP_MAIN_DOMAIN =
  process.env.NEXT_PUBLIC_APP_MAIN_DOMAIN ?? "localhost";
export const APP_WEBSITE =
  process.env.NEXT_PUBLIC_APP_WEBSITE ?? "http://localhost:3000";
export const APP_VERSION = process.env.npm_package_version ?? "1.0.0";
export const APP_LANGUAGE = process.env.NEXT_PUBLIC_APP_LANGUAGE ?? "pt-br";
export const APP_TIMEZONE =
  process.env.NEXT_PUBLIC_APP_TIMEZONE ?? "America/Sao_Paulo";
export const APP_DATETIME_FORMAT =
  process.env.NEXT_PUBLIC_APP_DATETIME_FORMAT ?? "lll";
export const APP_META_TITLE =
  process.env.NEXT_PUBLIC_APP_META_TITLE ?? "Marqtize";
export const APP_META_SLOGAN = process.env.NEXT_PUBLIC_APP_META_SLOGAN ?? "";
export const APP_META_TITLE_SEPARATOR =
  process.env.NEXT_PUBLIC_APP_META_TITLE_SEPARATOR ?? "|";
export const APP_META_KEYWORDS =
  process.env.NEXT_PUBLIC_APP_META_KEYWORDS ?? "";
export const APP_META_DESCRIPTION =
  process.env.NEXT_PUBLIC_APP_META_DESCRIPTION ?? "";

// * SOCIAL LINKS
export const EMAIL = process.env.NEXT_PUBLIC_EMAIL ?? "";
export const PHONE = process.env.NEXT_PUBLIC_PHONE ?? "";
export const MOBILE = process.env.NEXT_PUBLIC_MOBILE ?? "";
export const CNPJ = process.env.NEXT_PUBLIC_CNPJ ?? "";
export const ADDRESS = process.env.NEXT_PUBLIC_ADDRESS ?? "";
export const ADDRESS_LINE = process.env.NEXT_PUBLIC_ADDRESS_LINE ?? "";
export const CITY = process.env.NEXT_PUBLIC_CITY ?? "";
export const STATE = process.env.NEXT_PUBLIC_STATE ?? "";
export const STATE_UF = process.env.NEXT_PUBLIC_STATE_UF ?? "";
export const POSTCODE = process.env.NEXT_PUBLIC_POSTCODE ?? "";
export const WEBSITE = process.env.NEXT_PUBLIC_WEBSITE ?? "";
export const INSTAGRAM_URL = process.env.NEXT_PUBLIC_INSTAGRAM_URL ?? "";
export const FACEBOOK_URL = process.env.NEXT_PUBLIC_FACEBOOK_URL ?? "";
export const LICENSE_DATE = process.env.NEXT_PUBLIC_LICENSE_DATE ?? "";
export const RECAPTCHA_SITE_KEY =
  process.env.NEXT_PUBLIC_RECAPTCHA_SITE_KEY ?? "";

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
