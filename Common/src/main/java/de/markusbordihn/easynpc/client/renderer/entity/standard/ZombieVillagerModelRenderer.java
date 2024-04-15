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

package de.markusbordihn.easynpc.client.renderer.entity.standard;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.standard.StandardZombieVillagerModel;
import de.markusbordihn.easynpc.client.renderer.EasyNPCModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseHumanoidMobModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.ProfessionLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.VariantLayer;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class ZombieVillagerModelRenderer
    extends BaseHumanoidMobModelRenderer<
        ZombieVillager, Variant, StandardZombieVillagerModel<ZombieVillager>> {

  public static final ResourceLocation BASE_TEXTURE =
      new ResourceLocation("textures/entity/zombie_villager/zombie_villager.png");
  protected static final Map<Profession, ResourceLocation> TEXTURE_BY_PROFESSION =
      Util.make(
          new EnumMap<>(Profession.class),
          map -> {
            map.put(Profession.NONE, Constants.BLANK_ENTITY_TEXTURE);
            map.put(
                Profession.ARMORER,
                new ResourceLocation("textures/entity/zombie_villager/profession/armorer.png"));
            map.put(
                Profession.BUTCHER,
                new ResourceLocation("textures/entity/zombie_villager/profession/butcher.png"));
            map.put(
                Profession.CARTOGRAPHER,
                new ResourceLocation(
                    "textures/entity/zombie_villager/profession/cartographer.png"));
            map.put(
                Profession.CLERIC,
                new ResourceLocation("textures/entity/zombie_villager/profession/cleric.png"));
            map.put(
                Profession.FARMER,
                new ResourceLocation("textures/entity/zombie_villager/profession/farmer.png"));
            map.put(
                Profession.FISHERMAN,
                new ResourceLocation("textures/entity/zombie_villager/profession/fisherman.png"));
            map.put(
                Profession.FLETCHER,
                new ResourceLocation("textures/entity/zombie_villager/profession/fletcher.png"));
            map.put(
                Profession.LEATHERWORKER,
                new ResourceLocation(
                    "textures/entity/zombie_villager/profession/leatherworker.png"));
            map.put(
                Profession.LIBRARIAN,
                new ResourceLocation("textures/entity/zombie_villager/profession/librarian.png"));
            map.put(
                Profession.MASON,
                new ResourceLocation("textures/entity/zombie_villager/profession/mason.png"));
            map.put(
                Profession.NITWIT,
                new ResourceLocation("textures/entity/zombie_villager/profession/nitwit.png"));
            map.put(
                Profession.SHEPHERD,
                new ResourceLocation("textures/entity/zombie_villager/profession/shepherd.png"));
            map.put(
                Profession.TOOLSMITH,
                new ResourceLocation("textures/entity/zombie_villager/profession/toolsmith.png"));
            map.put(
                Profession.WEAPONSMITH,
                new ResourceLocation("textures/entity/zombie_villager/profession/weaponsmith.png"));
          });

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.DEFAULT, Constants.BLANK_ENTITY_TEXTURE);
            map.put(
                Variant.DESERT,
                new ResourceLocation("textures/entity/zombie_villager/type/desert.png"));
            map.put(
                Variant.JUNGLE,
                new ResourceLocation("textures/entity/zombie_villager/type/jungle.png"));
            map.put(
                Variant.PLAINS,
                new ResourceLocation("textures/entity/zombie_villager/type/plains.png"));
            map.put(
                Variant.SAVANNA,
                new ResourceLocation("textures/entity/zombie_villager/type/savanna.png"));
            map.put(
                Variant.SNOW,
                new ResourceLocation("textures/entity/zombie_villager/type/snow.png"));
            map.put(
                Variant.SWAMP,
                new ResourceLocation("textures/entity/zombie_villager/type/swamp.png"));
            map.put(
                Variant.TAIGA,
                new ResourceLocation("textures/entity/zombie_villager/type/taiga.png"));
          });

  public <L extends RenderLayer<ZombieVillager, StandardZombieVillagerModel<ZombieVillager>>>
      ZombieVillagerModelRenderer(
          EntityRendererProvider.Context context, Class<L> humanoidArmorLayerClass) {
    super(
        context,
        new StandardZombieVillagerModel<>(context.bakeLayer(ModelLayers.ZOMBIE_VILLAGER)),
        0.5F,
        BASE_TEXTURE);
    this.addLayer(
        EasyNPCModelRenderer.getHumanoidArmorLayer(
            this,
            context,
            ModelLayers.ZOMBIE_VILLAGER_INNER_ARMOR,
            ModelLayers.ZOMBIE_VILLAGER_OUTER_ARMOR,
            humanoidArmorLayerClass));
    this.addLayer(new VariantLayer<>(this, context.getModelSet(), TEXTURE_BY_VARIANT));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ProfessionLayer<>(this, context.getModelSet(), TEXTURE_BY_PROFESSION));
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    return BASE_TEXTURE;
  }

  @Override
  public void renderDefaultPose(
      ZombieVillager entity,
      StandardZombieVillagerModel<ZombieVillager> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-1.0D, 0.0D, 0.0D);
        poseStack.mulPose(Axis.YP.rotationDegrees(180f));
        poseStack.mulPose(Axis.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Axis.YP.rotationDegrees(270.0F));
        model.getHead().xRot = -0.7853982F;
        model.getHead().yRot = -0.7853982F;
        model.getHead().zRot = -0.7853982F;
        break;
      case SLEEPING:
        poseStack.translate(1.0D, 0.0D, 0.0D);
        break;
      default:
        model.getHead().xRot = 0F;
        model.getHead().yRot = 0F;
        model.getHead().zRot = 0F;
        break;
    }
  }
}
