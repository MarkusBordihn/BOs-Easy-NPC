/**
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

package de.markusbordihn.easynpc.client.renderer.entity;

import java.util.EnumMap;
import java.util.Map;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.npc.HumanoidSlim.Variant;

@OnlyIn(Dist.CLIENT)
public class HumanoidSlimRenderer extends MobRenderer<EasyNPCEntity, PlayerModel<EasyNPCEntity>> {

  // Variant Textures
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        map.put(Variant.ALEX, new ResourceLocation("textures/entity/alex.png"));
        map.put(Variant.KAWORRU,
            new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid_slim/kaworru.png"));
      });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.ALEX);

  public HumanoidSlimRenderer(EntityRendererProvider.Context context) {
    super(context, new PlayerModel<>(context.bakeLayer(ModelLayers.PLAYER_SLIM), true), 0.5F);
    this.addLayer(new HumanoidArmorLayer<>(this,
        new HumanoidModel<>(context.bakeLayer(ModelLayers.PLAYER_SLIM_INNER_ARMOR)),
        new HumanoidModel<>(context.bakeLayer(ModelLayers.PLAYER_SLIM_OUTER_ARMOR))));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCEntity entity) {
    switch (entity.getSkinType()) {
      case PLAYER_SKIN:
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
    return 7;
  }

}
