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

package de.markusbordihn.easynpc.entity.data;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;

import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.model.ModelPose;
import de.markusbordihn.easynpc.skin.SkinType;

public class CustomDataSerializers {

  public static final EntityDataSerializer<DialogType> DIALOG_TYPE =
      new EntityDataSerializer<DialogType>() {
        public void write(FriendlyByteBuf buffer, DialogType value) {
          buffer.writeEnum(value);
        }

        public DialogType read(FriendlyByteBuf buffer) {
          return buffer.readEnum(DialogType.class);
        }

        public DialogType copy(DialogType value) {
          return value;
        }
      };

  public static final EntityDataSerializer<ModelPose> MODEL_POSE =
      new EntityDataSerializer<ModelPose>() {
        public void write(FriendlyByteBuf buffer, ModelPose value) {
          buffer.writeEnum(value);
        }

        public ModelPose read(FriendlyByteBuf buffer) {
          return buffer.readEnum(ModelPose.class);
        }

        public ModelPose copy(ModelPose value) {
          return value;
        }
      };

  public static final EntityDataSerializer<Profession> PROFESSION =
      new EntityDataSerializer<Profession>() {
        public void write(FriendlyByteBuf buffer, Profession value) {
          buffer.writeEnum(value);
        }

        public Profession read(FriendlyByteBuf buffer) {
          return buffer.readEnum(Profession.class);
        }

        public Profession copy(Profession value) {
          return value;
        }
      };

  public static final EntityDataSerializer<SkinType> SKIN_TYPE =
      new EntityDataSerializer<SkinType>() {
        public void write(FriendlyByteBuf buffer, SkinType value) {
          buffer.writeEnum(value);
        }

        public SkinType read(FriendlyByteBuf buffer) {
          return buffer.readEnum(SkinType.class);
        }

        public SkinType copy(SkinType value) {
          return value;
        }
      };

  static {
    EntityDataSerializers.registerSerializer(DIALOG_TYPE);
    EntityDataSerializers.registerSerializer(MODEL_POSE);
    EntityDataSerializers.registerSerializer(PROFESSION);
    EntityDataSerializers.registerSerializer(SKIN_TYPE);
  }
}
