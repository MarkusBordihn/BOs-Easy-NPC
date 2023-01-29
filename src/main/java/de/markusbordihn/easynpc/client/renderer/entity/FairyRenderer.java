/**
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.FairyModel;
import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

@OnlyIn(Dist.CLIENT)
public class FairyRenderer extends HumanoidMobRenderer<EasyNPCEntity, FairyModel<EasyNPCEntity>> {

  private static final ResourceLocation DEFAULT_LOCATION =
      new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_green.png");

  public FairyRenderer(EntityRendererProvider.Context context) {
    super(context, new FairyModel<>(context.bakeLayer(ModModelLayers.FAIRY)), 0.3F);
  }

  @Override
  protected int getBlockLightLevel(EasyNPCEntity entity, BlockPos blockPos) {
    return 7;
  }

  @Override
  protected void scale(EasyNPCEntity entity, PoseStack poseStack, float unused) {
    poseStack.scale(0.4F, 0.4F, 0.4F);
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCEntity entity) {
    if (entity.hasTextureLocation()) {
      return entity.getTextureLocation();
    }
    return DEFAULT_LOCATION;
  }

}
