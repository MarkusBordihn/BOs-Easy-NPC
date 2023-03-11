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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;

import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.client.renderer.entity.layers.CrossedArmsItemLayer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.CustomVillagerModel;
import de.markusbordihn.easynpc.client.renderer.entity.layers.ProfessionLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.VariantLayer;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.entity.npc.Villager.Variant;
import de.markusbordihn.easynpc.model.ModelPose;

@OnlyIn(Dist.CLIENT)
public class VillagerRenderer
    extends MobRenderer<EasyNPCEntity, CustomVillagerModel<EasyNPCEntity>> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Base Texture
  public static final ResourceLocation BASE_TEXTURE =
      new ResourceLocation("textures/entity/villager/villager.png");

  // Professions Textures
  protected static final Map<Profession, ResourceLocation> TEXTURE_BY_PROFESSION =
      Util.make(new EnumMap<>(Profession.class), map -> {
        map.put(Profession.NONE, Constants.BLANK_ENTITY_TEXTURE);
        map.put(Profession.ARMORER,
            new ResourceLocation("textures/entity/villager/profession/armorer.png"));
        map.put(Profession.BUTCHER,
            new ResourceLocation("textures/entity/villager/profession/butcher.png"));
        map.put(Profession.CARTOGRAPHER,
            new ResourceLocation("textures/entity/villager/profession/cartographer.png"));
        map.put(Profession.CLERIC,
            new ResourceLocation("textures/entity/villager/profession/cleric.png"));
        map.put(Profession.FARMER,
            new ResourceLocation("textures/entity/villager/profession/farmer.png"));
        map.put(Profession.FISHERMAN,
            new ResourceLocation("textures/entity/villager/profession/fisherman.png"));
        map.put(Profession.FLETCHER,
            new ResourceLocation("textures/entity/villager/profession/fletcher.png"));
        map.put(Profession.LEATHERWORKER,
            new ResourceLocation("textures/entity/villager/profession/leatherworker.png"));
        map.put(Profession.LIBRARIAN,
            new ResourceLocation("textures/entity/villager/profession/librarian.png"));
        map.put(Profession.MASON,
            new ResourceLocation("textures/entity/villager/profession/mason.png"));
        map.put(Profession.NITWIT,
            new ResourceLocation("textures/entity/villager/profession/nitwit.png"));
        map.put(Profession.SHEPHERD,
            new ResourceLocation("textures/entity/villager/profession/shepherd.png"));
        map.put(Profession.TOOLSMITH,
            new ResourceLocation("textures/entity/villager/profession/toolsmith.png"));
        map.put(Profession.WEAPONSMITH,
            new ResourceLocation("textures/entity/villager/profession/weaponsmith.png"));
      });

  // Variant Textures
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        map.put(Variant.DEFAULT, Constants.BLANK_ENTITY_TEXTURE);
        map.put(Variant.DESERT, new ResourceLocation("textures/entity/villager/type/desert.png"));
        map.put(Variant.JUNGLE, new ResourceLocation("textures/entity/villager/type/jungle.png"));
        map.put(Variant.PLAINS, new ResourceLocation("textures/entity/villager/type/plains.png"));
        map.put(Variant.SAVANNA, new ResourceLocation("textures/entity/villager/type/savanna.png"));
        map.put(Variant.SNOW, new ResourceLocation("textures/entity/villager/type/snow.png"));
        map.put(Variant.SWAMP, new ResourceLocation("textures/entity/villager/type/swamp.png"));
        map.put(Variant.TAIGA, new ResourceLocation("textures/entity/villager/type/taiga.png"));
      });

  public VillagerRenderer(EntityRendererProvider.Context context) {
    super(context, new CustomVillagerModel<>(context.bakeLayer(ModelLayers.VILLAGER)), 0.5F);
    this.addLayer(new VariantLayer<>(this, context.getModelSet(), TEXTURE_BY_VARIANT));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ProfessionLayer<>(this, context.getModelSet(), TEXTURE_BY_PROFESSION));
    this.addLayer(new CrossedArmsItemLayer<>(this, context.getItemInHandRenderer()));
  }

  public ResourceLocation getProfessionTextureLocation(Enum<?> profession) {
    return TEXTURE_BY_PROFESSION.get(profession);
  }

  public ResourceLocation getVariantTextureLocation(Enum<?> variant) {
    return TEXTURE_BY_VARIANT.get(variant);
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCEntity entity) {
    switch (entity.getSkinType()) {
      case SECURE_REMOTE_URL:
      case INSECURE_REMOTE_URL:
        return PlayerTextureManager.getOrCreateTextureWithDefault(entity, BASE_TEXTURE);
      default:
        return BASE_TEXTURE;
    }
  }

  @Override
  protected void scale(EasyNPCEntity entity, PoseStack poseStack, float unused) {
    if (entity.isBaby()) {
      poseStack.scale(entity.getScaleX() * 0.5f, entity.getScaleY() * 0.5f,
          entity.getScaleZ() * 0.5f);
      this.shadowRadius = 0.25F;
    } else {
      poseStack.scale(entity.getScaleX(), entity.getScaleY(), entity.getScaleZ());
      this.shadowRadius = 0.5F;
    }
  }

  @Override
  public void render(EasyNPCEntity entity, float entityYaw, float partialTicks, PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer, int light) {
    CustomVillagerModel<EasyNPCEntity> playerModel = this.getModel();

    // Render additional poses
    if (entity.getModelPose() == ModelPose.DEFAULT) {

      switch (entity.getPose()) {
        case DYING:
          poseStack.translate(-1.0D, 0.0D, 0.0D);
          poseStack.mulPose(Axis.YP.rotationDegrees(180f));
          poseStack.mulPose(Axis.ZP.rotationDegrees(this.getFlipDegrees(entity)));
          poseStack.mulPose(Axis.YP.rotationDegrees(270.0F));
          playerModel.getHead().xRot = -0.7853982F;
          playerModel.getHead().yRot = -0.7853982F;
          playerModel.getHead().zRot = -0.7853982F;
          break;
        case SLEEPING:
          poseStack.translate(1.0D, 0.0D, 0.0D);
          break;
        default:
          playerModel.getHead().xRot = 0F;
          playerModel.getHead().yRot = 0F;
          playerModel.getHead().zRot = 0F;
          break;
      }
    }

    super.render(entity, entityYaw, partialTicks, poseStack, buffer, light);
  }

  @Override
  protected int getBlockLightLevel(EasyNPCEntity entity, BlockPos blockPos) {
    return 7;
  }

}
