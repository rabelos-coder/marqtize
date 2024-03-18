import { capitalize } from "lodash";
import { notFound } from "next/navigation";
import { getTranslations } from "next-intl/server";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { FIND_FIRST_POST } from "@/graphql/blogPost";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { createApolloClient } from "@/utils/apollo";
import { concatTitle } from "@/utils/helpers";

const client = createApolloClient();

export async function generateMetadata({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale });
  let title = concatTitle(t("blog.title"));

  const { data, error } = await client.query({
    query: FIND_FIRST_POST,
    variables: {
      where: {
        AND: [
          {
            publishedAt: {
              lte: new Date().toISOString(),
            },
          },
        ],
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) console.log(error?.message);

  const { findFirstBlogPost } = data;
  if (findFirstBlogPost) {
    title = concatTitle(capitalize(findFirstBlogPost.title));
  }

  return {
    title,
  };
}

export default async function BlogPostPage({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale });

  let title = t("blog.categories.title");

  const { data, error } = await client.query({
    query: FIND_FIRST_POST,
    variables: {
      where: {
        AND: [
          {
            publishedAt: {
              lte: new Date().toISOString(),
            },
          },
        ],
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) console.log(error?.message);

  const { findFirstBlogPost } = data;
  if (findFirstBlogPost) {
    title = capitalize(findFirstBlogPost.title);
  } else {
    notFound();
  }

  return (
    <LandingLayout navbarExpanded>
      <section className="bg-light py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={10} xl={8}>
              <div className="single-post">
                <h1>{title}</h1>
                <p className="lead">{findFirstBlogPost.resume}</p>
              </div>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
