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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.Profession;
import java.util.HashSet;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.item.trading.MerchantOffers;

public class CustomDataSerializers {

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
    EntityDataSerializers.registerSerializer(MERCHANT_OFFERS);
    EntityDataSerializers.registerSerializer(OBJECTIVE_DATA_SET);
    EntityDataSerializers.registerSerializer(PROFESSION);
    EntityDataSerializers.registerSerializer(STRING_HASH_SET);
    EntityDataSerializers.registerSerializer(TRADING_TYPE);
    EntityDataSerializers.registerSerializer(UUID_HASH_SET);
  }

  protected CustomDataSerializers() {
  }
}
