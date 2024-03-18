import { notFound } from "next/navigation";
import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { FIND_FIRST_TAG } from "@/graphql/blogTags";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { createApolloClient } from "@/utils/apollo";
import { concatTitle } from "@/utils/helpers";

const client = createApolloClient();

export async function generateMetadata({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale });

  let title = concatTitle(t("blog.tags.title"));

  const { data, error } = await client.query({
    query: FIND_FIRST_TAG,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) console.log(error?.message);

  const { findFirstBlogTag } = data;
  if (findFirstBlogTag) {
    title = concatTitle(findFirstBlogTag.name);
  }

  return {
    title,
  };
}

export default async function BlogTagPage({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale });

  const { data, error } = await client.query({
    query: FIND_FIRST_TAG,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) console.log(error?.message);

  let title = t("blog.tags.title");
  const { findFirstBlogTag } = data;
  if (findFirstBlogTag) {
    title = `#${findFirstBlogTag.name}`;
  } else {
    notFound();
  }

  return (
    <LandingLayout>
      <Header title={title} description={t("blog.tags.shortDescription")} />
      <section className="bg-white py-10">
        <Container className="px-5">
          <h1 className="pb-3">This is a basic content page.</h1>
          <p className="lead">
            You can use this page as a starting point to create your own custom
            pages, or choose an already built example page to start development!
          </p>
          <p>
            Lorem ipsum dolor sit, amet consectetur adipisicing elit. Qui
            quisquam animi temporibus ipsum iusto necessitatibus laudantium
            beatae. Eligendi dolorum laudantium numquam? Officiis nemo error
            animi aliquam dolor consequatur ducimus unde.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit. Qui
            repellat magni eaque beatae explicabo fugit placeat earum, dolores
            quaerat aperiam vero adipisci quidem minus officiis blanditiis unde?
            Incidunt, ea ad.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit.
            Perspiciatis sed illum soluta, quaerat et deleniti magnam
            laudantium, non omnis numquam quos placeat. Porro autem consectetur
            dolor minima voluptatum modi maiores.
          </p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
