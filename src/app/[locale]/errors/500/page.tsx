"use client";

import { useState } from "react";
import { Button, Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";

export default function ErrorPage() {
  const [user, setUser] = useState({ name: "John Doe" });

  return (
    <LandingLayout>
      <Header
        title="Error Page"
        description="Create beautiful pages with easy to edit content"
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <h1>This is a error content page.</h1>
          <p>Name: {user.name}</p>
          <Button
            type="button"
            color="primary"
            onClick={() => setUser(undefined as any)}
          >
            Click me
          </Button>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
