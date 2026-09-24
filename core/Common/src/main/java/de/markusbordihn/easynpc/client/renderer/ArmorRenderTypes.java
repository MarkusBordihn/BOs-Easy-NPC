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

import com.mojang.renderpearl.api.pipeline.BlendFunction;
import com.mojang.renderpearl.api.pipeline.ColorTargetState;
import com.mojang.renderpearl.api.pipeline.RenderPipeline;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.iris.IrisManager;
import java.util.function.Function;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.client.renderer.rendertype.LayeringTransform;
import net.minecraft.client.renderer.rendertype.RenderSetup;
import net.minecraft.client.renderer.rendertype.RenderType;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Util;

/** Translucent armor render type, which was removed from vanilla with Minecraft 26.3. */
public final class ArmorRenderTypes {

  private static final RenderPipeline ARMOR_TRANSLUCENT_PIPELINE =
      RenderPipelineCopy.builder(
              RenderPipelines.ARMOR_CUTOUT_NO_CULL,
              Identifier.fromNamespaceAndPath(Constants.MOD_ID, "pipeline/armor_translucent"))
          .withColorTargetState(new ColorTargetState(BlendFunction.TRANSLUCENT))
          .build();

  private static final Function<Identifier, RenderType> ARMOR_TRANSLUCENT =
      Util.memoize(
          texture ->
              RenderType.create(
                  Constants.MOD_PREFIX_ID + "armor_translucent",
                  RenderSetup.builder(ARMOR_TRANSLUCENT_PIPELINE)
                      .withTexture(RenderTypeTextureAccessor.PRIMARY_TEXTURE_SAMPLER, texture)
                      .useLightmap()
                      .useOverlay()
                      .setLayeringTransform(LayeringTransform.VIEW_OFFSET_Z_LAYERING)
                      .affectsCrumbling()
                      .sortOnUpload()
                      .setOutline(RenderSetup.OutlineProperty.AFFECTS_OUTLINE)
                      .createRenderSetup()));

  static {
    IrisManager.copyShaderMapping(RenderPipelines.ARMOR_CUTOUT_NO_CULL, ARMOR_TRANSLUCENT_PIPELINE);
  }

  private ArmorRenderTypes() {}

  public static RenderType armorTranslucent(Identifier texture) {
    return ARMOR_TRANSLUCENT.apply(texture);
  }
}
