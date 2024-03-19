import { getTranslations } from "next-intl/server";

import { redirect } from "@/navigation";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("blog.categories.title"));

  return {
    title,
  };
}

export default function BlogCategoryPage({ params: { slug } }: any) {
  redirect(`/blog/category/${slug}/1`);
}
