/*
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.easynpc.client.model.custom;

import de.markusbordihn.easynpc.client.model.base.BaseHumanoidModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.CubeListBuilder;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.client.model.geom.builders.MeshDefinition;
import net.minecraft.client.model.geom.builders.PartDefinition;
import net.minecraft.world.entity.LivingEntity;

public class OrcModel<T extends LivingEntity> extends BaseHumanoidModel<T> {

  public OrcModel(ModelPart modelPart) {
    super(modelPart);
    this.hat.visible = false;
  }

  public static LayerDefinition createBodyLayer() {
    MeshDefinition meshDefinition = HumanoidModel.createMesh(CubeDeformation.NONE, 0F);
    PartDefinition partDefinition = meshDefinition.getRoot();

    // Head
    PartDefinition head =
        partDefinition.addOrReplaceChild(
            "head",
            CubeListBuilder.create()
                .texOffs(26, 17)
                .addBox(-4.0F, -8.0F, -4.0F, 8.0F, 8.0F, 8.0F, new CubeDeformation(0.0F))
                .texOffs(0, 0)
                .addBox(-4.5F, -8.5F, -4.5F, 9.0F, 8.0F, 9.0F, new CubeDeformation(0.0F))
                .texOffs(27, 0)
                .addBox(-4.5F, -4.0F, -5.0F, 9.0F, 5.0F, 4.0F, new CubeDeformation(0.0F))
                .texOffs(0, 0)
                .addBox(-1.0F, -4.0F, -5.0F, 2.0F, 2.0F, 1.0F, new CubeDeformation(0.0F)),
            PartPose.offset(0.0F, 0.0F, 0.0F));

    // Ears
    PartDefinition ears =
        head.addOrReplaceChild("ears", CubeListBuilder.create(), PartPose.offset(0.0F, 0.0F, 0.0F));
    ears.addOrReplaceChild(
        "ear_left",
        CubeListBuilder.create()
            .texOffs(0, 56)
            .addBox(0.0F, -5.5F, 0.0F, 4.0F, 11.0F, 0.0F, new CubeDeformation(0.0F)),
        PartPose.offsetAndRotation(4.0F, -5.5F, -0.5F, 0.0F, -0.3927F, 0.0F));
    ears.addOrReplaceChild(
        "ear_right",
        CubeListBuilder.create()
            .texOffs(8, 56)
            .addBox(-4.0F, -5.5F, 0.0F, 4.0F, 11.0F, 0.0F, new CubeDeformation(0.0F)),
        PartPose.offsetAndRotation(-4.0F, -5.5F, -0.5F, 0.0F, 0.3927F, 0.0F));

    // Body
    partDefinition.addOrReplaceChild(
        "body",
        CubeListBuilder.create()
            .texOffs(26, 33)
            .addBox(-4.0F, 0.0F, -2.0F, 8.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(0, 17)
            .addBox(-4.0F, 0.0F, -2.5F, 8.0F, 18.0F, 5.0F, new CubeDeformation(0.5F)),
        PartPose.offset(0.0F, 0.0F, 0.0F));

    // Arms
    partDefinition.addOrReplaceChild(
        "left_arm",
        CubeListBuilder.create()
            .texOffs(32, 49)
            .addBox(-1.0F, -2.0F, -2.0F, 3.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(48, 4)
            .addBox(-1.0F, -2.5F, -2.5F, 4.0F, 8.0F, 5.0F, new CubeDeformation(0.0F)),
        PartPose.offset(5.0F, 2.0F, 0.0F));

    partDefinition.addOrReplaceChild(
        "right_arm",
        CubeListBuilder.create()
            .texOffs(32, 49)
            .addBox(-2.0F, -2.0F, -2.0F, 3.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(53, 28)
            .addBox(-3.0F, -2.5F, -2.5F, 4.0F, 8.0F, 5.0F, new CubeDeformation(0.0F)),
        PartPose.offset(-5.0F, 2.0F, 0.0F));

    // Legs
    partDefinition.addOrReplaceChild(
        "left_leg",
        CubeListBuilder.create()
            .texOffs(0, 40)
            .addBox(-2.1F, 0.0F, -2.0F, 4.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(46, 45)
            .addBox(-2.1F, 0.0F, -2.0F, 4.0F, 12.0F, 4.0F, new CubeDeformation(0.0F)),
        PartPose.offset(-1.9F, 12.0F, 0.0F));
    partDefinition.addOrReplaceChild(
        "right_leg",
        CubeListBuilder.create()
            .texOffs(0, 40)
            .addBox(-2.1F, 0.0F, -2.0F, 4.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(16, 49)
            .addBox(-2.1F, 0.0F, -2.0F, 4.0F, 12.0F, 4.0F, new CubeDeformation(0.0F)),
        PartPose.offset(2.1F, 12.0F, 0.0F));

    return LayerDefinition.create(meshDefinition, 128, 128);
  }
}
