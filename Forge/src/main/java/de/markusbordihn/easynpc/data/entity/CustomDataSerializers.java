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

package de.markusbordihn.easynpc.data.entity;

import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.Profession;
import java.util.HashSet;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.item.trading.MerchantOffers;

public class CustomDataSerializers {

  public static final EntityDataSerializer<ActionEventSet> ACTION_EVENT_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, ActionEventSet value) {
          buffer.writeNbt(value.createTag());
        }

        public ActionEventSet read(FriendlyByteBuf buffer) {
          return new ActionEventSet(buffer.readNbt());
        }

        public ActionEventSet copy(ActionEventSet value) {
          return value;
        }
      };
  public static final EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, DialogDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public DialogDataSet read(FriendlyByteBuf buffer) {
          return new DialogDataSet(buffer.readNbt());
        }

        public DialogDataSet copy(DialogDataSet value) {
          return value;
        }
      };
  public static final EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, ObjectiveDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public ObjectiveDataSet read(FriendlyByteBuf buffer) {
          return new ObjectiveDataSet(buffer.readNbt());
        }

        public ObjectiveDataSet copy(ObjectiveDataSet value) {
          return value;
        }
      };
  public static final EntityDataSerializer<TradingType> TRADING_TYPE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, TradingType value) {
          buffer.writeEnum(value);
        }

        public TradingType read(FriendlyByteBuf buffer) {
          return buffer.readEnum(TradingType.class);
        }

        public TradingType copy(TradingType value) {
          return value;
        }
      };
  public static final EntityDataSerializer<ModelPose> MODEL_POSE =
      new EntityDataSerializer<>() {
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
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, MerchantOffers value) {
          buffer.writeNbt(value.createTag());
        }

        public MerchantOffers read(FriendlyByteBuf buffer) {
          return new MerchantOffers(buffer.readNbt());
        }

        public MerchantOffers copy(MerchantOffers value) {
          return value;
        }
      };
  public static final EntityDataSerializer<CustomPosition> POSITION =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, CustomPosition position) {
          buffer.writeFloat(position.x());
          buffer.writeFloat(position.y());
          buffer.writeFloat(position.z());
        }

        public CustomPosition read(FriendlyByteBuf buffer) {
          return new CustomPosition(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        public CustomPosition copy(CustomPosition position) {
          return position;
        }
      };
  public static final EntityDataSerializer<CustomScale> SCALE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, CustomScale scale) {
          buffer.writeFloat(scale.x());
          buffer.writeFloat(scale.y());
          buffer.writeFloat(scale.z());
        }

        public CustomScale read(FriendlyByteBuf buffer) {
          return new CustomScale(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        public CustomScale copy(CustomScale scale) {
          return scale;
        }
      };
  public static final EntityDataSerializer<Profession> PROFESSION =
      new EntityDataSerializer<>() {
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
      new EntityDataSerializer<>() {
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
  public static final EntityDataSerializer<HashSet<String>> STRING_HASH_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, HashSet<String> value) {
          for (String entry : value) {
            buffer.writeUtf(entry);
          }
        }

        public HashSet<String> read(FriendlyByteBuf buffer) {
          HashSet<String> value = new HashSet<>();
          while (buffer.isReadable()) {
            value.add(buffer.readUtf());
          }
          return value;
        }

        public HashSet<String> copy(HashSet<String> value) {
          return value;
        }
      };
  public static final EntityDataSerializer<HashSet<UUID>> UUID_HASH_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, HashSet<UUID> value) {
          for (UUID entry : value) {
            buffer.writeUUID(entry);
          }
        }

        public HashSet<UUID> read(FriendlyByteBuf buffer) {
          HashSet<UUID> value = new HashSet<>();
          while (buffer.isReadable()) {
            value.add(buffer.readUUID());
          }
          return value;
        }

        public HashSet<UUID> copy(HashSet<UUID> value) {
          return value;
        }
      };

  static {
    EntityDataSerializers.registerSerializer(ACTION_EVENT_SET);
    EntityDataSerializers.registerSerializer(DIALOG_DATA_SET);
    EntityDataSerializers.registerSerializer(MERCHANT_OFFERS);
    EntityDataSerializers.registerSerializer(MODEL_POSE);
    EntityDataSerializers.registerSerializer(OBJECTIVE_DATA_SET);
    EntityDataSerializers.registerSerializer(POSITION);
    EntityDataSerializers.registerSerializer(SCALE);
    EntityDataSerializers.registerSerializer(PROFESSION);
    EntityDataSerializers.registerSerializer(SKIN_TYPE);
    EntityDataSerializers.registerSerializer(STRING_HASH_SET);
    EntityDataSerializers.registerSerializer(TRADING_TYPE);
    EntityDataSerializers.registerSerializer(UUID_HASH_SET);
  }

  protected CustomDataSerializers() {
  }
}
