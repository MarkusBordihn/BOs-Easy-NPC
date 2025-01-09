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
import de.markusbordihn.easynpc.client.model.standard.StandardVillagerModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.ProfessionLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.VariantLayer;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CrossedArmsItemLayer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class VillagerModelRenderer
    extends BaseMobModelRenderer<Villager, Variant, StandardVillagerModel<Villager>> {

  public static final ResourceLocation BASE_TEXTURE =
      ResourceLocation.withDefaultNamespace("textures/entity/villager/villager.png");
  protected static final Map<Profession, ResourceLocation> TEXTURE_BY_PROFESSION =
      Util.make(
          new EnumMap<>(Profession.class),
          map -> {
            map.put(Profession.NONE, Constants.BLANK_ENTITY_TEXTURE);
            map.put(
                Profession.ARMORER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/armorer.png"));
            map.put(
                Profession.BUTCHER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/butcher.png"));
            map.put(
                Profession.CARTOGRAPHER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/cartographer.png"));
            map.put(
                Profession.CLERIC,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/cleric.png"));
            map.put(
                Profession.FARMER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/farmer.png"));
            map.put(
                Profession.FISHERMAN,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/fisherman.png"));
            map.put(
                Profession.FLETCHER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/fletcher.png"));
            map.put(
                Profession.LEATHERWORKER,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/leatherworker.png"));
            map.put(
                Profession.LIBRARIAN,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/librarian.png"));
            map.put(
                Profession.MASON,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/mason.png"));
            map.put(
                Profession.NITWIT,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/nitwit.png"));
            map.put(
                Profession.SHEPHERD,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/shepherd.png"));
            map.put(
                Profession.TOOLSMITH,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/toolsmith.png"));
            map.put(
                Profession.WEAPONSMITH,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/villager/profession/weaponsmith.png"));
          });
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.DEFAULT, Constants.BLANK_ENTITY_TEXTURE);
            map.put(
                Variant.DESERT,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/desert.png"));
            map.put(
                Variant.JUNGLE,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/jungle.png"));
            map.put(
                Variant.PLAINS,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/plains.png"));
            map.put(
                Variant.SAVANNA,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/savanna.png"));
            map.put(
                Variant.SNOW,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/snow.png"));
            map.put(
                Variant.SWAMP,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/swamp.png"));
            map.put(
                Variant.TAIGA,
                ResourceLocation.withDefaultNamespace("textures/entity/villager/type/taiga.png"));
          });

  public VillagerModelRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardVillagerModel<>(context.bakeLayer(ModelLayers.VILLAGER)),
        0.5F,
        BASE_TEXTURE);
    this.addLayer(new VariantLayer<>(this, context.getModelSet(), TEXTURE_BY_VARIANT));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ProfessionLayer<>(this, context.getModelSet(), TEXTURE_BY_PROFESSION));
    this.addLayer(new CrossedArmsItemLayer<>(this, context.getItemInHandRenderer()));
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    return BASE_TEXTURE;
  }

  @Override
  public void renderDefaultPose(
      Villager entity,
      StandardVillagerModel<Villager> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-1.0D, 0.0D, 0.0D);
        poseStack.mulPose(Axis.YP.rotationDegrees(180f));
        poseStack.mulPose(Axis.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Axis.YP.rotationDegrees(270.0F));
        this.getModel().getHead().xRot = -0.7853982F;
        this.getModel().getHead().yRot = -0.7853982F;
        this.getModel().getHead().zRot = -0.7853982F;
        break;
      case SLEEPING:
        poseStack.translate(1.0D, 0.0D, 0.0D);
        break;
      default:
        this.getModel().getHead().xRot = 0F;
        this.getModel().getHead().yRot = 0F;
        this.getModel().getHead().zRot = 0F;
        break;
    }
  }
}
