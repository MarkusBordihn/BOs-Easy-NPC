/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.renderer;

import com.mojang.blaze3d.vertex.VertexConsumer;

public class OpacityVertexConsumer implements VertexConsumer {

  private final VertexConsumer delegate;
  private final float alpha;

  public OpacityVertexConsumer(VertexConsumer delegate, float alpha) {
    this.delegate = delegate;
    this.alpha = alpha;
  }

  @Override
  public VertexConsumer vertex(double x, double y, double z) {
    this.delegate.vertex(x, y, z);
    return this;
  }

  @Override
  public VertexConsumer color(int red, int green, int blue, int vertexAlpha) {
    this.delegate.color(red, green, blue, Math.round(vertexAlpha * this.alpha));
    return this;
  }

  @Override
  public VertexConsumer uv(float u, float v) {
    this.delegate.uv(u, v);
    return this;
  }

  @Override
  public VertexConsumer overlayCoords(int u, int v) {
    this.delegate.overlayCoords(u, v);
    return this;
  }

  @Override
  public VertexConsumer uv2(int u, int v) {
    this.delegate.uv2(u, v);
    return this;
  }

  @Override
  public VertexConsumer normal(float x, float y, float z) {
    this.delegate.normal(x, y, z);
    return this;
  }

  @Override
  public void endVertex() {
    this.delegate.endVertex();
  }

  @Override
  public void defaultColor(int red, int green, int blue, int defaultAlpha) {
    this.delegate.defaultColor(red, green, blue, Math.round(defaultAlpha * this.alpha));
  }

  @Override
  public void unsetDefaultColor() {
    this.delegate.unsetDefaultColor();
  }
}
