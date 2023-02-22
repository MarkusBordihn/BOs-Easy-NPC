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

import java.util.EnumMap;
import java.util.Map;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.Util;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.FairyModel;
import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.npc.Fairy.Variant;

@OnlyIn(Dist.CLIENT)
public class FairyRenderer extends HumanoidMobRenderer<EasyNPCEntity, FairyModel<EasyNPCEntity>> {

  // Variant Textures
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        map.put(Variant.BLUE,
            new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_blue.png"));
        map.put(Variant.GREEN,
            new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_green.png"));
        map.put(Variant.RED,
            new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_red.png"));
      });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.GREEN);

  public FairyRenderer(EntityRendererProvider.Context context) {
    super(context, new FairyModel<>(context.bakeLayer(ModModelLayers.FAIRY)), 0.3F);
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCEntity entity) {
    switch (entity.getSkinType()) {
      case SECURE_REMOTE_URL:
      case INSECURE_REMOTE_URL:
        return PlayerTextureManager.getOrCreateTextureWithDefault(entity, DEFAULT_TEXTURE);
      default:
        return TEXTURE_BY_VARIANT.getOrDefault(entity.getVariant(), DEFAULT_TEXTURE);
    }
  }

  @Override
  protected void scale(EasyNPCEntity entity, PoseStack poseStack, float unused) {
    if (entity.isBaby()) {
      poseStack.scale(entity.getScaleX() * 0.5f, entity.getScaleY() * 0.5f,
          entity.getScaleZ() * 0.5f);
    } else {
      poseStack.scale(entity.getScaleX(), entity.getScaleY(), entity.getScaleZ());
    }
  }

  @Override
  protected int getBlockLightLevel(EasyNPCEntity entity, BlockPos blockPos) {
    return 10;
  }

}
