/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity.layers;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.api.model.CustomModelConfig;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.Identifier;

public abstract class CustomModelRenderLayer<
        S extends LivingEntityRenderState, M extends EntityModel<? super S>>
    extends RenderLayer<S, M> {

  protected final M customModel;
  protected final CustomModelConfig config;
  protected final RenderLayerParent<S, M> renderer;

  protected CustomModelRenderLayer(
      RenderLayerParent<S, M> renderer, M customModel, CustomModelConfig config) {
    super(renderer);
    this.renderer = renderer;
    this.customModel = customModel;
    this.config = config;
  }

  @Override
  public void submit(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      S renderState,
      float limbSwing,
      float limbSwingAmount) {
    if (this.customModel == null) {
      return;
    }

    // Get the texture location based on config
    Identifier textureLocation = getTextureLocation(renderState);

    // Setup animation state for the custom model
    this.customModel.setupAnim(renderState);

    // Render the custom model using the new API
    // Using coloredCutoutModelCopyLayerRender for proper rendering with invisibility support
    RenderLayer.coloredCutoutModelCopyLayerRender(
        this.customModel,
        textureLocation,
        poseStack,
        submitNodeCollector,
        packedLight,
        renderState,
        -1, // white color (no tint)
        config.renderMode().ordinal() // render type from config
        );
  }

  protected Identifier getTextureLocation(S renderState) {
    if (config.shouldUseEntityTexture()
        && this.renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      return easyNPCRenderer.getTextureFromRenderState(renderState);
    } else if (config.shouldUseVariantTexture()
        && this.renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      return easyNPCRenderer.getTextureFromRenderState(renderState);
    }
    return config.getCustomTexture();
  }
}
